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
parent pName pChild pChildren = Parent <$>
                                pName     >: "name"     <*>
                                pChild    >: "child"    <*>
                                pChildren >: "children"

child :: V String32 -> V Child
child cName = Child <$> cName >: "name"

-- # Aeson instances
instance FromJSON (V Int) where
  parseJSON (Number n) = (pure . pure . floor) n
  parseJSON _          = pure incorrectType

instance FromJSON (V String32) where
  parseJSON = withText "V String32" $ \t -> pure $ string32 $ T.unpack t

instance FromJSON (V Child) where
  parseJSON a =
    let parse    o     = validate <$> (o .:: "name")
        validate cName = child (cName .>: "name")
    in case a of
      (Object o) -> parse o
      _          -> pure incorrectType

instance FromJSON (V Parent) where
  parseJSON a =
    let parse o = validate
                  <$> o .::  "name"
                  <*> o .::? "child"
                  <*> o .::  "children"
        validate pName pChild pChildren = parent
                                          (pName     .>: "name")
                                          (pChild    .>: "child")
                                          (pChildren .>: "children")
    in case a of
      (Object o) -> parse o
      _          -> pure incorrectType