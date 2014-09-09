{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Models where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.Text as T


import Validation

data Parent = Parent { parentName     :: String32
                     , parentChild    :: Child
                     , parentChildren :: [Child]
                     } deriving (Show)
 
data Child = Child { childName :: String32 
                   } deriving (Show)
 
instance FromJSON (V Child) where
  parseJSON a = withObject "V Child" parse a
    where parse o = validate <$> o .: "name"
          validate name _ = Child <$> 
                            name (JsonError "name" a)

instance FromJSON (V [Child]) where
  parseJSON = withArraySeqV "V [Child]"

instance FromJSON (V Parent) where
  parseJSON a = withObject "V Parent" parse a
    where parse o = validate <$> 
                     o .: "name" <*> 
                     o .: "child" <*>
                     o .: "children"
          validate name child children _ = Parent <$> 
                                name (JsonError "name" a)  <*> 
                                child (JsonError "child" a) <*>
                                children (JsonError "children" a)
