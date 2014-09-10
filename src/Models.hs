{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Models where

import           Control.Applicative
import           Data.Aeson
import           Data.Semigroup
import qualified Data.Text as T
import           Data.Validation.Historical
import           Data.Validation.Aeson

import Validation

data Parent = Parent { parentName     :: String32
                     , parentChild    :: Maybe Child
                     , parentChildren :: [Child]
                     } deriving (Show)
 
data Child = Child { childName :: String32 
                   } deriving (Show)


-- # Smart constructors
parent :: V String32 -> V (Maybe Child) -> V [Child] -> V Parent
parent pName pChild pChildren = Parent <$>
                                pName .+ [Env "name"] <*>                                
                                pChild .+ [Env "child"] <*>                                
                                pChildren .+ [Env "children"]

child :: V String32 -> V Child
child cName = Child <$> cName .+ [Env "name"]


-- # Aeson instances
instance FromJSON (V Int) where
  parseJSON (Number n) = (pure . pure . floor) n
  parseJSON _          = pure incorrectTypeError

instance FromJSON (V String32) where
  parseJSON = withText "V String32" $ \t -> pure $ string32 $ T.unpack t

instance FromJSON (V Child) where
  parseJSON a = 
    let parse o = validate 
                  <$> (o .:: "name")
        validate cName = child 
                         (cName .+ [JsonKey $ "name"])
    in case a of
      (Object o) -> parse o
      _          -> pure incorrectTypeError


instance FromJSON (V [Child]) where
  parseJSON = parseArray incorrectTypeError (\i -> [JsonIndex i]) "V [Child]"


instance FromJSON (V Parent) where
  parseJSON a = 
    let parse o = validate 
                  <$> o .:: "name"
                  <*> o .::? "child"
                  <*> o .::  "children"
        validate pName pChild pChildren = parent 
                                          (pName .+ [JsonKey $ "name"])
                                          (pChild .+ [JsonKey $ "child"])
                                          (pChildren .+ [JsonKey $ "children"])
    in case a of
      (Object o) -> parse o
      _          -> pure incorrectTypeError