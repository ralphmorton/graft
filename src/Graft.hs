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
graft name vars = do
    (GraftData tpls) <- view graftData
    maybe (throwG GraftMissingTemplate) (graftTpl vars) (M.lookup name tpls)

graftTpl :: (MonadError e m, AsGraftError e, TemplateData a) => a -> Template -> m T.Text
graftTpl vars tpl = do
    let ox = compile vars <$> tpl
    let res = foldM (\s -> either Left (Right . mappend s)) mempty ox
    case res of
        Left e -> throwG e
        Right r -> return r

compile :: TemplateData a => a -> Part -> Either GraftError T.Text
compile _ (Lit t) = Right t
compile vm (Bind key) = case (child k vm) of
    Nothing -> Left $ GraftMissingVariable key
    Just v -> case (resolve v kx) of
        Left e -> Left e
        Right (Val val) -> Right val
        Right _ -> Left $ GraftVariableMismatch key
    where
    (k:kx) = T.splitOn "." key
compile vm (Loop bound key tpl) = case (child k vm) of
    Nothing -> Left $ GraftMissingVariable key
    Just v -> case (resolve v kx) of
        Left e -> Left e
        Right (Array vx) -> do
            tx <- mapM (compileLoop bound tpl vm) vx
            return $ mconcat tx
        Right _ -> Left $ GraftVariableMismatch key
    where
    (k:kx) = T.splitOn "." key

compileLoop :: (TemplateData a, TemplateData b) => T.Text -> Template -> a -> b -> Either GraftError T.Text
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
constructTemplate (CLoopEnd:cx) = Left "Unexpected loop end"
constructTemplate (CLoopStart b v:cx) = do
    (loop, cx') <- constructLoop b v cx
    (loop:) <$> constructTemplate cx'

constructLoop :: T.Text -> T.Text -> [Chunk] -> Either String (Part, [Chunk])
constructLoop bound var cx = do
    (lx, rx) <- takeLoopContents 0 cx
    tpl <- constructTemplate lx
    return (Loop bound var tpl, rx)

takeLoopContents :: Int -> [Chunk] -> Either String ([Chunk], [Chunk])
takeLoopContents n [] = Left "Unterminated loop"
takeLoopContents n (c@(CLoopStart _ _):rx) = do
    (cx, rx') <- takeLoopContents (n + 1) rx
    return (c:cx, rx')
takeLoopContents n (CLoopEnd:rx)
    | n - 1 < 0 = return ([], rx)
    | otherwise = do
        (cx, rx') <- takeLoopContents (n - 1) rx
        return (CLoopEnd:cx, rx')
takeLoopContents n (c:rx) = do
    (cx, rx') <- takeLoopContents n rx
    return (c:cx, rx')

parseTemplate :: Parser [Chunk]
parseTemplate = many1 (lit <|> mbind <|> mloop)

lit :: Parser Chunk
lit = CLit . T.pack <$> many1 (noneOf "{[")

mbind :: Parser Chunk
mbind = (try bind) <|> rchunk
    where
    rchunk = do
        char '{'
        rv <- many $ noneOf "{["
        return . CLit . T.pack $ ("{" ++ rv)

bind :: Parser Chunk
bind = do
    string "{{"
    v <- T.strip . T.pack <$> many1 (noneOf "}")
    string "}}"
    return (CBind v)

mloop :: Parser Chunk
mloop = (try loopEnd) <|>(try loopStart) <|>  rchunk
    where
    rchunk = do
        char '['
        rv <- many $ noneOf "{["
        return . CLit . T.pack $ ("[" ++ rv)

loopStart :: Parser Chunk
loopStart = do
    string "[["
    b <- T.strip . T.pack <$> many1 (noneOf ":")
    char ':'
    n <- T.strip . T.pack <$> many1 (noneOf "]")
    string "]]"
    return $ CLoopStart b n

loopEnd :: Parser Chunk
loopEnd = do
    try (string "[[")
    many (char ' ')
    string "end"
    many (char ' ')
    string "]]"
    return CLoopEnd
