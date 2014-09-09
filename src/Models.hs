{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Models where

import           Control.Applicative
import           Control.Lens ((#))
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Semigroup
import qualified Data.Text as T
import Data.Validation

import Validation

data Parent = Parent { parentName     :: String32
                     , parentChild    :: Child
                     , parentChildren :: [Child]
                     } deriving (Show)
 
data Child = Child { childName :: String32 
                   } deriving (Show)
 
instance FromJSON (V Child) where
  parseJSON a = 
    let parse o = validate <$> 
                  retrieve "name" o a
        validate name c = Child <$> 
                          name (c <> JsonError "name" [KeyString "name"] a)
    in case a of 
      (Object o) -> parse o
      _          -> pure $ wrongType


instance FromJSON (V [Child]) where
  parseJSON a = case a of
    (Array _) -> withArraySeqV "V [Child]" a
    _         -> pure $ wrongType 

instance FromJSON (V Parent) where
  parseJSON a =
    let parse o = validate <$> 
                  retrieve "name" o a <*> 
                  retrieve "child" o a <*>
                  retrieve "children" o a
        validate name child children c = Parent <$> 
                                         name (c <> JsonError "name" [KeyString "name"] a)  <*> 
                                         child (c <> JsonError "child" [KeyString "child"] a) <*>
                                         children (c <> JsonError "children" [KeyString "children"] a)
    in case a of
      (Object o) -> withObject "V Parent" parse a
      _          -> pure $ wrongType


missingKey :: T.Text -> Value -> V a
missingKey name a c = _Failure # [JsonKeyNotFound (JsonError name [KeyString name] a)]

wrongType :: V a
wrongType c = _Failure # [JsonIncorrectValueType c]


retrieve rKey rObj a = (rObj .:? rKey .!= missingKey rKey a)