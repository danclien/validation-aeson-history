{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Validation where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.Text as T
import           Data.Validation
import           Data.Validation.Aeson
import           Data.Validation.Historical

-- # Concrete validation types
type VEnv = T.Text

data VError = MustNotBeEmpty
            | MustBeLessThan32Length String
            deriving (Eq, Show)

type V a = AesonV T.Text VError a

-- # Base types
newtype String32 = String32 String deriving (Eq, Show)

string32 :: String -> V String32
string32 t = asksV f
  where f c
          | null t         = _Failure # verror c MustNotBeEmpty
          | length t > 32  = _Failure # verror c (MustBeLessThan32Length t)
          | otherwise      = _Success # String32 t

-- # Aeson instances
instance FromJSON (V String32) where
  parseJSON = withText "V String32" $ \t -> pure $ string32 $ T.unpack t