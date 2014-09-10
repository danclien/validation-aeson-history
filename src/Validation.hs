{-# LANGUAGE FlexibleContexts #-}

module Validation where

import           Control.Lens
import qualified Data.Text as T
import           Data.Validation
import           Data.Validation.Aeson
import           Data.Validation.Historical

-- # Concrete validation type

type VEnv = T.Text

data VError = MustNotBeEmpty String
            | MustBeLessThan32Length String
            deriving (Eq, Show)

type V a = AesonV T.Text VError a

-- # Base types
newtype String32 = String32 String deriving (Eq, Show)

string32 :: String -> V String32
string32 t = asksV f
  where f c
          | null t         = _Failure # single c (MustNotBeEmpty t)
          | length t > 32  = _Failure # single c (MustBeLessThan32Length t)
          | otherwise      = _Success # String32 t

