{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Validation where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.Text as T
import           Data.Validation
import           Data.Validation.Aeson
import           Data.Validation.Historical
import Data.Functor.Compose.Reader

-- # Concrete validation types
type VEnv = T.Text

data VError = MustNotBeEmpty String
            | MustBeLessThan32Length String
            deriving (Eq, Show)

type V a = AesonV2 T.Text VError a

--type V a = UserV T.Text VError a
type VA a = AesonV2 T.Text VError a

type V3 f g a = V2 f g T.Text VError a

--type V a = HistoricalV f internalLog V.Vector log V.Vector internalError err a

--type AesonV2 log err a =
--  HistoricalV V.Vector AesonVLog V.Vector log V.Vector AesonVError err a

-- # Base types
newtype String32 = String32 String deriving (Eq, Show)

string32 :: String -> V3 f g String32
string32 t = reader f
  where f c
          | null t         = _Failure # errorH c (MustNotBeEmpty t)
          | length t > 32  = _Failure # errorH c (MustBeLessThan32Length t)
          | otherwise      = _Success # String32 t

-- # Aeson instances
instance FromJSON (VA String32) where
  parseJSON = withText "V String32" $ \t -> pure $ string32 $ T.unpack t