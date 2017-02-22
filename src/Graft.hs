{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Graft(
    module Graft.Types,
    loadDirectory,
    attachTemplate,
    graft
) where

import Graft.Types

import Control.Lens (review, view)
import Control.Monad (filterM, foldM)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (break)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import System.Directory (doesFileExist, getDirectoryContents)
import Text.Parsec
import Text.Parsec.String

data Contain a = forall a. TemplateData a => C a

instance TemplateData (Contain a) where
    child k (C a) = child k a

-- |Compile a template
graft :: (MonadError e m, AsGraftError e, MonadReader r m, HasGraftData r, TemplateData a) => String -> a -> m T.Text
graft name vars = graftTpl vars =<< lookupTpl name

lookupTpl :: (MonadError e m, AsGraftError e, MonadReader r m, HasGraftData r) => String -> m Template
lookupTpl name = do
    (GraftData tpls) <- view graftData
    maybe (throwG $ GraftMissingTemplate name) return (M.lookup name tpls)

graftTpl :: (MonadError e m, AsGraftError e, MonadReader r m, HasGraftData r, TemplateData a) => a -> Template -> m T.Text
graftTpl vars tpl = do
    ox <- mapM (compile vars) tpl
    return $ mconcat ox

compile :: (MonadError e m, AsGraftError e, MonadReader r m, HasGraftData r, TemplateData a) => a -> Part -> m T.Text
compile _ (Lit t) = return t
compile vm (Bind key) = case (child k vm) of
    Nothing -> throwG $ GraftMissingVariable key
    Just v -> case (resolve v kx) of
        Left e -> throwG e
        Right (Val val) -> return val
        Right _ -> throwG $ GraftVariableMismatch key
    where
    (k:kx) = T.splitOn "." key
compile vm (Loop bound key tpl) = case (child k vm) of
    Nothing -> throwG $ GraftMissingVariable key
    Just v -> case (resolve v kx) of
        Left e -> throwG e
        Right (Array vx) -> mconcat <$> mapM (compileLoop bound tpl vm) vx
        Right _ -> throwG $ GraftVariableMismatch key
    where
    (k:kx) = T.splitOn "." key
compile vm (Conditional key tpl) = case (child k vm) of
    Nothing -> pure ""
    Just v -> case (resolve v kx) of
        Right _ -> graftTpl vm tpl
        _ -> pure ""
    where
    (k:kx) = T.splitOn "." key
compile vm (SubTemplate t False) = graftTpl vm =<< lookupTpl (T.unpack t)
compile vm (SubTemplate key True) = case (child k vm) of
    Nothing -> throwG $ GraftMissingVariable key
    Just v -> case (resolve v kx) of
        Left e -> throwG e
        Right (Val val) -> graftTpl vm =<< lookupTpl (T.unpack val)
        Right _ -> throwG $ GraftVariableMismatch key
    where
    (k:kx) = T.splitOn "." key

compileLoop :: (MonadError e m, AsGraftError e, MonadReader r m, HasGraftData r, TemplateData a, TemplateData b) => T.Text -> Template -> a -> b -> m T.Text
compileLoop bound tpl a b = do
    let bmap = M.fromList [(bound, Object b)]
    let dta = [C bmap, C a]
    graftTpl dta tpl

resolve :: Var -> [T.Text] -> Either GraftError Var
resolve v [] = Right v
resolve (Object o) (k:kx) = case (child k o) of
    Nothing -> Left $ GraftMissingVariable k
    Just v -> resolve v kx
resolve _ (k:_) = Left $ GraftVariableMismatch k

throwG :: (MonadError e m, AsGraftError e) => GraftError -> m a
throwG = throwError . review _GraftError

-- |Load all templates from a directory
loadDirectory :: MonadIO m => String -> m (Either String GraftData)
loadDirectory dir = liftIO $ do
    let sdir = reverse . dropWhile (=='/') $ reverse dir
    contents <- fmap ((sdir++"/")++) <$> getDirectoryContents dir
    files <- filterM doesFileExist contents
    rx <- mapM loadTpl files
    return $ foldM (\g -> uncurry $ attachTemplate g) mempty rx

loadTpl :: MonadIO m => String -> m (String, String)
loadTpl path = liftIO $ do
    let fname = reverse . takeWhile (\c -> c /= '/' && c /= '\\') $ reverse path
    dta <- readFile path
    return (fname, dta)

-- |Attach a new template
attachTemplate :: GraftData -> String -> String -> Either String GraftData
attachTemplate (GraftData tpls) tpln tpl = GraftData . flip (M.insert tpln) tpls <$> pres
    where
    pres = case (parse parseTemplate "" tpl) of
        Left e -> Left $ show e
        Right cx -> constructTemplate cx

constructTemplate :: [Chunk] -> Either String Template
constructTemplate [] = return []
constructTemplate (CLit s:cx) = (Lit s:) <$> constructTemplate cx
constructTemplate (CBind v:cx) = (Bind v:) <$> constructTemplate cx
constructTemplate (CControlEnd:cx) = Left "Unexpected control flow end"
constructTemplate (CLoopStart b v:cx) = do
    (loop, cx') <- constructLoop b v cx
    (loop:) <$> constructTemplate cx'
constructTemplate (CConditionalStart v:cx) = do
    (conditional, cx') <- constructConditional v cx
    (conditional:) <$> constructTemplate cx'
constructTemplate (CSubTemplate t ref:cx) = (SubTemplate t ref:) <$> constructTemplate cx

constructLoop :: T.Text -> T.Text -> [Chunk] -> Either String (Part, [Chunk])
constructLoop bound var cx = do
    (lx, rx) <- takeControlContents 0 cx
    tpl <- constructTemplate lx
    return (Loop bound var tpl, rx)

constructConditional :: T.Text -> [Chunk] -> Either String (Part, [Chunk])
constructConditional var cx = do
    (cx, rx) <- takeControlContents 0 cx
    tpl <- constructTemplate cx
    return (Conditional var tpl, rx)

takeControlContents :: Int -> [Chunk] -> Either String ([Chunk], [Chunk])
takeControlContents n [] = Left "Unterminated control flow structure"
takeControlContents n (c@(CLoopStart _ _):rx) = do
    (cx, rx') <- takeControlContents (n + 1) rx
    return (c:cx, rx')
takeControlContents n (c@(CConditionalStart _):rx) = do
    (cx, rx') <- takeControlContents (n + 1) rx
    return (c:cx, rx')
takeControlContents n (CControlEnd:rx)
    | n - 1 < 0 = return ([], rx)
    | otherwise = do
        (cx, rx') <- takeControlContents (n - 1) rx
        return (CControlEnd:cx, rx')
takeControlContents n (c:rx) = do
    (cx, rx') <- takeControlContents n rx
    return (c:cx, rx')

parseTemplate :: Parser [Chunk]
parseTemplate = many1 (lit <|> mbind <|> mcontrol <|> msub)

lit :: Parser Chunk
lit = CLit . T.pack <$> many1 (noneOf "{[<")

mbind :: Parser Chunk
mbind = (try bind) <|> rchunk
    where
    rchunk = do
        char '{'
        rv <- many $ noneOf "{[<"
        return . CLit . T.pack $ ("{" ++ rv)

bind :: Parser Chunk
bind = do
    string "{{"
    v <- T.strip . T.pack <$> many1 (noneOf "}")
    string "}}"
    return (CBind v)

mcontrol :: Parser Chunk
mcontrol = (try controlEnd) <|> (try controlStart) <|> rchunk
    where
    rchunk = do
        char '['
        rv <- many $ noneOf "{[<"
        return . CLit . T.pack $ ("[" ++ rv)

controlStart :: Parser Chunk
controlStart = do
    string "[["
    c <- control
    string "]]"
    return c

control :: Parser Chunk
control = try conditionalStart <|> try loopStart

loopStart :: Parser Chunk
loopStart = do
    skipMany space
    b <- T.strip . T.pack <$> many1 (noneOf ":")
    char ':'
    n <- T.strip . T.pack <$> many1 (noneOf "]")
    return (CLoopStart b n)

conditionalStart :: Parser Chunk
conditionalStart = do
    skipMany space
    _ <- char '?'
    n <- T.strip . T.pack <$> many1 (noneOf "]")
    return (CConditionalStart n)

controlEnd :: Parser Chunk
controlEnd = do
    try (string "[[")
    many (char ' ')
    string "end"
    many (char ' ')
    string "]]"
    return CControlEnd

msub :: Parser Chunk
msub = (try sub) <|> rchunk
    where
    rchunk = do
        char '<'
        rv <- many $ noneOf "{[<"
        return . CLit . T.pack $ ("<" ++ rv)

sub :: Parser Chunk
sub = do
    string "<|"
    isRef <- try (const True <$> char '@') <|> (pure False)
    v <- T.strip . T.pack <$> many1 (noneOf "|")
    string "|>"
    return (CSubTemplate v isRef)
