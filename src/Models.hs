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
import qualified Data.Vector as V

import Validation

data Parent = Parent { parentName     :: String32
                     , parentChild    :: Maybe Child
                     , parentChildren :: [Child]
                     } deriving (Show)
 
data Child = Child { childName :: String32 
                   } deriving (Show)



env :: T.Text -> V.Vector (AesonVEnv T.Text)
env a = V.singleton (Env a)

jsonKey :: T.Text -> V.Vector (AesonVEnv a)
jsonKey a = V.singleton (JsonKey a)

jsonIndex :: Int -> V.Vector (AesonVEnv a)
jsonIndex a = V.singleton (JsonIndex a)

-- # Smart constructors
parent :: V String32 -> V (Maybe Child) -> V [Child] -> V Parent
parent pName pChild pChildren = Parent <$>
                                pName .+ (env "name") <*>                      
                                pChild .+ (env "child") <*>
                                pChildren .+ (env "children")

child :: V String32 -> V Child
child cName = Child <$> cName .+ (env "name")


-- # Aeson instances
instance FromJSON (V Int) where
  parseJSON (Number n) = (pure . pure . floor) n
  parseJSON _          = pure incorrectType

instance FromJSON (V String32) where
  parseJSON = withText "V String32" $ \t -> pure $ string32 $ T.unpack t

instance FromJSON (V Child) where
  parseJSON a = 
    let parse o = validate 
                  <$> (o .:: "name")
        validate cName = child 
                         (cName .+ (jsonKey "name"))
    in case a of
      (Object o) -> parse o
      _          -> pure incorrectType


instance FromJSON (V [Child]) where
  parseJSON = parseArray incorrectType (\i -> (jsonIndex i)) "V [Child]"


instance FromJSON (V Parent) where
  parseJSON a = 
    let parse o = validate 
                  <$> o .:: "name"
                  <*> o .::? "child"
                  <*> o .::  "children"
        validate pName pChild pChildren = parent 
                                          (pName .+ (jsonKey $ "name"))
                                          (pChild .+ (jsonKey $ "child"))
                                          (pChildren .+ (jsonKey $ "children"))
    in case a of
      (Object o) -> parse o
      _          -> pure incorrectType