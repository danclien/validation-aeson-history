{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Validation where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.Text as T
import           Data.Validation
import           Data.Validation.Aeson
import           Data.Validation.Reader

-- # Concrete validation types
data VError a = MustNotBeEmpty a String
              | MustBeLessThan32Length a String
              deriving (Eq, Show)

type V = ReaderAccValidation [T.Text] [VError [T.Text]]

-- # Base types
newtype String32 = String32 String deriving (Eq, Show)

string32 :: String -> V String32
string32 t = reader f
  where f c
          | null t         = _Failure # [MustNotBeEmpty c t]
          | length t > 32  = _Failure # [MustBeLessThan32Length c t]
          | otherwise      = _Success # String32 t

-- # Aeson instances
--instance FromJSON (V String32) where
--  parseJSON = withText "V String32" $ \t -> pure $ string32 $ T.unpack t