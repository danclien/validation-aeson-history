{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Models where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T
import           Data.Validation.Aeson

import Validation

-- # Models
data Parent = Parent { parentName     :: String32
                     , parentChild    :: Maybe Child
                     , parentChildren :: [Child]
                     } deriving (Show)

data Child = Child { childName :: String32
                   } deriving (Show)

-- # Smart constructors
parent :: V String32 -> V (Maybe Child) -> V [Child] -> V Parent
parent pName pChild pChildren = Parent
                                <$> pName     >: "name"
                                <*> pChild    >: "child"
                                <*> pChildren >: "children"

child :: V String32 -> V Child
child cName = Child <$> cName >: "name"

-- # Aeson instances
instance FromJSON (V String32) where
  parseJSON = withText "V String32" $ \t -> pure $ string32 $ T.unpack t

instance FromJSON (V Child) where
  parseJSON = withObjectV parse
    where parse o        = child <$> (o .:: "name")

instance FromJSON (V Parent) where
  parseJSON = withObjectV parse
    where parse o = parent
                    <$> o .::  "name"
                    <*> o .::? "child"
                    <*> o .::  "children"