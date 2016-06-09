
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

-- |Attach a new template
attachTemplate :: GraftData -> String -> String -> Either String GraftData
attachTemplate (GraftData tpls) tpln tpl = GraftData . flip (M.insert tpln) tpls <$> pres
    where
    pres = case (parse parseTemplate "" tpl) of
        Left e -> Left $ show e
        Right r -> Right r

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

graft :: (MonadError e m, AsGraftError e, MonadReader r m, HasGraftData r) => String -> VarMap -> m T.Text
graft name vars = do
    (GraftData tpls) <- view graftData
    flip (maybe (throwG GraftMissingTemplate)) (M.lookup name tpls) $ \tpl -> do
        let ox = compile vars <$> tpl
        let res = foldM (\s -> either Left (Right . mappend s)) mempty ox
        case res of
            Left e -> throwG e
            Right r -> return r

compile :: VarMap -> Chunk -> Either GraftError T.Text
compile _ (Lit t) = Right t
compile vm (Bind k) = case (M.lookup k vm) of
    Nothing -> Left $ GraftMissingVariable (T.unpack k)
    Just val -> Right val

throwG :: (MonadError e m, AsGraftError e) => GraftError -> m a
throwG = throwError . review _GraftError

parseTemplate :: Parser Template
parseTemplate = many1 (lit <|> mvar)

lit :: Parser Chunk
lit = Lit . T.pack <$> many1 (noneOf "{")

mvar :: Parser Chunk
mvar = (try var) <|> rchunk
    where
    rchunk = do
        char '{'
        rv <- many $ noneOf "{"
        return . Lit . T.pack $ ("{" ++ rv)

var :: Parser Chunk
var = do
    string "{{"
    v <- T.strip . T.pack <$> many1 (noneOf "}")
    string "}}"
    return (Bind v)
