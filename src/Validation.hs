{-# LANGUAGE FlexibleContexts #-}

module Validation where

import           Control.Lens
import           Data.Aeson
import qualified Data.Aeson.Types as AT
import qualified Data.Text as T
import           Data.Validation
import           Data.Validation.Aeson
import           Data.Validation.Historical

-- # Concrete validation type

data VPath = JsonPath T.Text
             | CustomPath T.Text
             deriving (Eq, Show)

data VError a = MustNotBeEmpty a String
              | MustBeLessThan32Length a String
              | JsonKeyNotFound a
              | JsonIncorrectValueType a
              deriving (Eq, Show)

type V a = AccValidationH [VPath] [VError [VPath]] a

-- # Concrete error types
incorrectTypeError :: V a
incorrectTypeError = asksV $ \c -> _Failure # [JsonIncorrectValueType c]

missingKeyError :: V a
missingKeyError = asksV $ \c -> _Failure # [JsonKeyNotFound c]

-- # Concrete helper method
(.::) :: FromJSON (V a) => Object -> T.Text -> AT.Parser (V a)
obj .:: key = obj .:? key .!= missingKeyError
{-# INLINE (.::) #-}



withObjectV = withObjectV' incorrectTypeError


-- # Base types
newtype String32 = String32 String deriving (Eq, Show)

string32 :: String -> V String32
string32 t = asksV f
  where f c
          | null t         = _Failure # [MustNotBeEmpty c t]
          | length t > 32  = _Failure # [MustBeLessThan32Length c t]
          | otherwise      = _Success # String32 t
