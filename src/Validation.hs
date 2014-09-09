{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Validation where


import           Control.Applicative
import           Control.Lens ((#))
import           Data.Semigroup
import           Data.Aeson
import           Data.Aeson.Encode
import qualified Data.Aeson.Types as A
import           Data.Validation
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Traversable as TR
import qualified Data.Vector as V


data JsonKey = KeyString T.Text
             | KeyIndex Int
             deriving (Eq, Show)

data JsonError = JsonNoError 
               | JsonError { key :: T.Text
                           , path :: [JsonKey]
                           , source :: A.Value
                           }

instance Semigroup JsonError where
  a                              <> JsonNoError                    = a
  JsonNoError                    <> b                              = b
  (JsonError keyA pathA sourceA) <> (JsonError keyB pathB sourceB) = JsonError keyB (mappend pathA pathB) sourceB


instance Show JsonError where
  show JsonNoError = ""
  show (JsonError k p s) = "{\r\n  key: " ++ 
                         show k ++ 
                         ",\r\n  path: " ++
                         show p ++ 
                         ",\r\n  source: " ++
                         show (encodeViaText s) ++ 
                         "\r\n}"
    where encodeViaText = TLB.toLazyText . encodeToTextBuilder . toJSON

type VHistory = JsonError

type V a = VHistory -> AccValidation [VError VHistory] a
 
data VError a = MustNotBeEmpty a String
              | MustBeLessThan32Length a String
              | JsonKeyNotFound a
              | JsonIncorrectValueType a
              deriving (Eq, Show)

-- Simple
newtype String32 = String32 String deriving (Eq, Show)

string32 :: String -> V String32
string32 t i
  | null t         = _Failure # [MustNotBeEmpty i t]
  | length t > 32  = _Failure # [MustBeLessThan32Length i t]
  | otherwise      = _Success # String32 t

instance FromJSON (V String32) where
  parseJSON = withText "V String32" $ \t -> pure $ string32 $ T.unpack t

withArraySeqV s a = withArray s parse a
    where parse = fmap (sequenceV a) . mapM parseJSON . V.toList

sequenceV :: Applicative f => Value -> [JsonError -> f a] -> VHistory -> f [a]
sequenceV a xs c = TR.sequenceA xs'
  where is = zip xs [0..]
        xs' = fmap (\x -> fst x (c <> JsonError (T.pack $ show $ snd x) [KeyIndex $ snd x] a)) is
