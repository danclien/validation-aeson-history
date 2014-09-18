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
import           Data.Validation.Compose
import           Data.Validation.Parser
import           Data.Functor.Compose


-- # Concrete validation types
type VEnv = T.Text

data VError = MustNotBeEmpty String
            | MustBeLessThan32Length String
            deriving (Eq, Show)


type V a = HistoricalV [T.Text] [VError] a

type VP a = AesonV [T.Text] [VError] a

-- # Base types
newtype String32 = String32 String deriving (Eq, Show)

string32 :: String -> V String32
string32 t = reader f
  where f c
          | null t         = _Failure # [MustNotBeEmpty t]
          | length t > 32  = _Failure # [MustBeLessThan32Length t]
          | otherwise      = _Success # String32 t

-- # Aeson instances
instance FromJSON (VP String32) where
  parseJSON = withText "VP String32" $ \t -> pure $
    liftInnerV $ string32 $ T.unpack t





