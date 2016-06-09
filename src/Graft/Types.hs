{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Graft.Types where

import Control.Lens (Lens')
import Control.Lens.TH
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)

data GraftData = GraftData {
    _graftDataTemplates :: M.Map String Template
} deriving Show

type Template = [Chunk]
type VarMap = M.Map Text Text

data Chunk
    = Lit Text
    | Bind Text
    deriving Show

data GraftError
    = GraftMissingTemplate
    | GraftMissingVariable String
    deriving Show

instance Monoid GraftData where
    mempty = GraftData M.empty
    mappend (GraftData ta) (GraftData tb) = GraftData $ M.union ta tb

makeClassy ''GraftData
makeClassyPrisms ''GraftError
