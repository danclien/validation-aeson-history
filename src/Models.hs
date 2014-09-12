{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Models where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Validation
import           Data.Validation.Parser
import           Data.Validation.Reader
import qualified Data.Text as T
-- import           Data.Validation.Aeson as VA

import Validation

-- # Models
data Parent = Parent { parentName     :: String32
                     , parentChild    :: Child
                     --, parentChildren :: [Child]
                     } deriving (Eq, Show)

data Child = Child { childName :: String32
                   } deriving (Eq, Show)

-- # Smart constructors
parent :: V String32 -> V Child -> V Parent
parent pName pChild = Parent
                                <$> pName     >: "name"
                                <*> pChild    >: "child"
                                -- <*> pChildren >: "children"

child :: V String32 -> V Child
child cName = Child
              <$> cName >: "name"

type JsonV a = [T.Text] -> V a

instance FromJSON (JsonV String32) where
  parseJSON = withText "V String32" $ \t -> getParser $ pure $ string32 $ T.unpack t


instance FromJSON (JsonV Child) where
  parseJSON (Object o) = getParser $
                           child
                           <$> (o .:: "name")
  parseJSON _          = undefined


instance FromJSON (JsonV Parent) where
  parseJSON (Object o) = getParser $
                           parent
                           <$> (o .:: "name")
                           <*> (o .:: "child")
                           -- <*> (o .:: "children")
  parseJSON _          = undefined


(.::) :: (FromJSON ([T.Text] -> a)) =>
  Object
  -> T.Text
  -> ParserReader [T.Text] a
obj .:: key = (liftParser $ obj .: key) <>:: [key]

-- Parser ReaderAccValidation ReaderAccValidation

-- type VA = ReaderReaderAccValidation [T.Text] [T.Text] [VError]

--workingtest :: V String32 -> VA Child
--workingtest cName = reader $ child
--                             <$> (pure cName)



--test2 :: VA String32
--test2 = lift $ string32 "test2"



-- # Aeson instances
--instance FromJSON (V Child) where
--  parseJSON = withObjectV parse
--    where parse o = child
--                    <$> o .:: "name"

--instance FromJSON (V Parent) where
--  parseJSON = withObjectV parse
--    where parse o = parent
--                    <$> o .::  "name"
--                    <*> o .::? "child"
--                    <*> o .::  "children"