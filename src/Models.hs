{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Models where

import           Control.Applicative
import           Data.Aeson
import           Data.Validation.Aeson as VA
import           Data.Validation.Historical as H

import Validation

-- # Models
data Parent = Parent { parentName     :: String32
                     , parentChild    :: Maybe Child
                     , parentChildren :: [Child]
                     } deriving (Eq, Show)

data Child = Child { childName :: String32
                   } deriving (Eq, Show)

-- # Smart constructors
parent :: V String32 -> V (Maybe Child) -> V [Child] -> V Parent
parent pName pChild pChildren = Parent
                                <$> pName     VA.>: "name"
                                <*> pChild    VA.>: "child"
                                <*> pChildren VA.>: "children"

child :: V String32 -> V Child
child cName = Child
              <$> cName VA.>: "name"

-- # Aeson instances
instance FromJSON (V Child) where
  parseJSON = withObjectV parse
    where parse o = child
                    <$> o .:: "name"

instance FromJSON (V Parent) where
  parseJSON = withObjectV parse
    where parse o = parent
                    <$> o .::  "name"
                    <*> o .::? "child"
                    <*> o .::  "children"