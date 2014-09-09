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
                     , parentChild    :: Child
                     , parentChildren :: [Child]
                     } deriving (Show)
 
data Child = Child { childName :: String32 
                   } deriving (Show)

-- ## Smart constructors
parent :: V String32 -> V Child -> V [Child] -> V Parent
parent pName pChild pChildren  = asksV f
  where f c = Parent <$>
              runV pName     (c <> ["name"])     <*>
              runV pChild    (c <> ["child"])    <*>
              runV pChildren (c <> ["children"])

child :: V String32 -> V Child
child cName = asksV f
  where f c = Child <$> 
              runV cName (c <> ["name"])

-- ## Aeson instances
instance FromJSON (V String32) where
  parseJSON = withText "V String32" $ \t -> pure $ string32 $ T.unpack t

instance FromJSON (V Child) where
  parseJSON = parseObject1 incorrectTypeError Child $ \o -> 
    (o .:: "name", ["name"])

instance FromJSON (V [Child]) where
  parseJSON = parseArray incorrectTypeError (\i -> [T.pack $ show i]) "V [Child]"

instance FromJSON (V Parent) where
  parseJSON = parseObject3 incorrectTypeError Parent $ \o -> 
    ( (o .:: "name",     ["name"]     )
    , (o .:: "child",    ["child"]    )
    , (o .:: "children", ["children"] )
    )

