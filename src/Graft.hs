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

compile :: TemplateData a => a -> Chunk -> Either GraftError T.Text
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
        Right r -> Right r

parseTemplate :: Parser Template
parseTemplate = many1 (lit <|> mbind <|> mloop)

lit :: Parser Chunk
lit = Lit . T.pack <$> many1 (noneOf "{[")

mbind :: Parser Chunk
mbind = (try bind) <|> rchunk
    where
    rchunk = do
        char '{'
        rv <- many $ noneOf "{["
        return . Lit . T.pack $ ("{" ++ rv)

bind :: Parser Chunk
bind = do
    string "{{"
    v <- T.strip . T.pack <$> many1 (noneOf "}")
    string "}}"
    return (Bind v)

mloop :: Parser Chunk
mloop = (try loop) <|> rchunk
    where
    rchunk = do
        char '['
        rv <- many $ noneOf "{["
        return . Lit . T.pack $ ("[" ++ rv)

loop :: Parser Chunk
loop = do
    (b, n) <- loopStart
    dta <- manyTill anyChar loopEnd
    case (parse parseTemplate "" dta) of
        Left e -> fail (show e)
        Right tpl -> return (Loop b n tpl)


loopStart :: Parser (T.Text, T.Text)
loopStart = do
    string "[["
    b <- T.strip . T.pack <$> many1 (noneOf ":")
    char ':'
    n <- T.strip . T.pack <$> many1 (noneOf "]")
    string "]]"
    return (b, n)

loopEnd :: Parser ()
loopEnd = do
    try (string "[[")
    many (char ' ')
    string "end"
    many (char ' ')
    string "]]"
    return ()
