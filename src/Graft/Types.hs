{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Graft.Types where

import Control.Applicative ((<|>))
import Control.Lens (Lens')
import Control.Lens.TH
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)

class TemplateData a where
    child :: Text -> a -> Maybe Var

data GraftData = GraftData {
    _graftDataTemplates :: M.Map String Template
} deriving Show

type Template = [Part]
type VarMap = M.Map Text Var

data Chunk
    = CLit Text
    | CBind Text
    | CLoopStart Text Text --name bound_as
    | CLoopEnd
    | CSubTemplate Text
    deriving (Eq, Show)

data Part
    = Lit Text
    | Bind Text
    | Loop Text Text Template --name bound_as subtemplate
    | SubTemplate Text
    deriving Show

data Var where
    Val :: Text -> Var
    Object :: TemplateData a => a -> Var
    Array :: TemplateData a => [a] -> Var

data GraftError
    = GraftMissingTemplate String
    | GraftMissingVariable Text
    | GraftVariableMismatch Text
    deriving Show

instance Monoid GraftData where
    mempty = GraftData M.empty
    mappend (GraftData ta) (GraftData tb) = GraftData $ M.union ta tb

instance TemplateData a => TemplateData [a] where
    child _ [] = Nothing
    child k (d:dx) = child k d <|> child k dx

instance TemplateData VarMap where
    child = M.lookup

makeClassy ''GraftData
makeClassyPrisms ''GraftError
