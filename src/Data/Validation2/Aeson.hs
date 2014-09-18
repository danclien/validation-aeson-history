{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Validation.Aeson where

import Control.Applicative
import Control.Lens
import Data.Functor.Compose.Reader
import Data.Semigroup
import Data.Validation
import Data.Validation.Parser
import Data.Validation.Reader
import Data.Aeson.Types as AT
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

import Data.Validation.Historical

instance Semigroup (V.Vector a) where
  (<>) = mappend

data AesonVLog = JsonKey T.Text
               | JsonIndex Int
               deriving (Eq, Show)

data AesonVError = AesonKeyNotFound
                 | AesonIncorrectType
                 deriving (Eq, Show)

type V2 f g log err a = HistoricalV V.Vector f V.Vector log V.Vector g err a

-- type V3 log err a = HistoricalV _ _ V.Vector log V.Vector _ err a

type AesonV2 log err a =
  HistoricalV V.Vector AesonVLog V.Vector log V.Vector AesonVError err a

-- # Default errors

incorrectTypeError :: AesonV2 log err a
incorrectTypeError = internalErrorH AesonIncorrectType

keyNotFoundError :: AesonV2 log err a
keyNotFoundError = internalErrorH AesonKeyNotFound

---- # Reader functions inside Parser

appendJsonKeyP :: Parser (AesonV2 log err a)
                  -> T.Text
                  -> Parser (AesonV2 log err a)
appendJsonKeyP a key = appendP a $ jsonKey key

---- # Helpers

withObjectV :: Applicative f =>
  (Object -> f (AesonV2 env err a))
  -> Value
  -> f (AesonV2 env err a)
withObjectV parse a =
  case a of
    (Object o) -> parse o
    _          -> pure incorrectTypeError

--verror :: V.Vector (AesonVEnv env) -> err -> V.Vector (AesonVError env err)
--verror env err = V.singleton $ ValidationError env err

jsonKey :: (Monoid (g b), Applicative f) =>
  T.Text
  -> Log f g AesonVLog b
jsonKey a = Log (pure (JsonKey a)) mempty

jsonIndex :: (Monoid (g b), Applicative f) =>
  Int
  -> Log f g AesonVLog b
jsonIndex a = Log (pure (JsonIndex a)) mempty

(>:) :: AesonV2 log err a -> log -> AesonV2 log err a
a >: env = a *<> (Log mempty (pure env))
{-# INLINE (>:) #-}

---- # JSON parsing combinators
(.::) :: FromJSON (AesonV2 env err a) =>
  Object
  -> T.Text
  -> AT.Parser (AesonV2 env err a)
obj .:: key = appendJsonKeyP (obj .:? key .!= keyNotFoundError) key
{-# INLINE (.::) #-}

(.::?) :: FromJSON (AesonV2 env err a) =>
  H.HashMap T.Text Value
  -> T.Text
  -> AT.Parser (AesonV2 env err (Maybe a))
obj .::? key = case H.lookup key obj of
                  Nothing -> pure $ pure Nothing
                  Just a  -> appendJsonKeyP (fmap Just <$> parseJSON a) key
{-# INLINE (.::?) #-}

---- # Sequencing
withArraySeqV :: (FromJSON t, Applicative f) =>
  (t -> Int -> f a)
  -> Value
  -> AT.Parser (f [a])
withArraySeqV f = withArray "V [a]" parse
  where parse = fmap (sequenceRC f) . mapM parseJSON . V.toList

---- # FromJSON instances
instance FromJSON (AesonV2 env err Int) where
  parseJSON (Number n) = (pure . pure . floor) n
  parseJSON _          = pure incorrectTypeError

instance FromJSON (AesonV2 env err a) => FromJSON (AesonV2 env err [a]) where
  parseJSON a = case a of
    (Array _) -> withArraySeqV (\env i -> env *<> jsonIndex i) a
    _         -> pure incorrectTypeError


-- ********************************************************************************
-- Parser
-- ********************************************************************************

localP :: (log -> log)
          -> Parser (ReaderAccValidation log err a)
          -> Parser (ReaderAccValidation log err a)
localP f = fmap (local f)


appendP :: Semigroup log =>
           Parser (ReaderAccValidation log err a)
           -> log
           -> Parser (ReaderAccValidation log err a)
appendP a log = localP (<> log) a
