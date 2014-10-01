{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Validation.Aeson where

import Control.Applicative
import Control.Lens
import Data.Functor.Compose
import Data.Functor.Compose.Reader
import Data.Semigroup
import Data.Validation
import Data.Validation.Compose
import Data.Validation.Parser
import Data.Aeson
--import Data.Validation.Historical
import Data.Aeson.Types as AT
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.Traversable as TR

--import Data.Validation.Historical

instance Semigroup (V.Vector a) where
  (<>) = mappend

data AesonVLog = JsonKey T.Text
               | JsonIndex Int
               deriving (Eq, Show)

data AesonVError = AesonKeyNotFound
                 | AesonIncorrectType
                 deriving (Eq, Show)

type AesonV log err = ParserV AesonVLog AesonVError log err

---- # Default errors

incorrectTypeError :: AesonV log err a
incorrectTypeError = internalError AesonIncorrectType

keyNotFoundError :: AesonV log err a
keyNotFoundError = internalError AesonKeyNotFound

------ # Reader functions inside Parser

--appendJsonKeyP :: AT.Parser (AesonV log err a)
--                  -> T.Text
--                  -> AT.Parser (AesonV log err a)
appendJsonKeyP a key = appendP a $ jsonKey key

------ # Helpers

withObjectV :: Applicative f =>
  (Object -> f (AesonV env err a))
  -> Value
  -> f (AesonV env err a)
withObjectV parse a =
  case a of
    (Object o) -> parse o
    _          -> pure incorrectTypeError

----verror :: V.Vector (AesonVEnv env) -> err -> V.Vector (AesonVError env err)
----verror env err = V.singleton $ ValidationError env err

--jsonKey :: (Monoid (g b), Applicative f) =>
--  T.Text
--  -> Log f g AesonVLog b
jsonKey a = Log (pure (JsonKey a)) mempty

--jsonIndex :: (Monoid (g b), Applicative f) =>
--  Int
--  -> Log f g AesonVLog b
jsonIndex a = Log (pure (JsonIndex a)) mempty

--(>:) :: AesonV2 log err a -> log -> AesonV2 log err a
--a >: env = a *<> (Log mempty (pure env))
--{-# INLINE (>:) #-}

------ # JSON parsing combinators
(.::) :: FromJSON (AesonV log err a) =>
  Object
  -> T.Text
  -> Parser (AesonV log err a)
obj .:: key = appendJsonKeyP (obj .:? key .!= keyNotFoundError) key
{-# INLINE (.::) #-}

(.::?) :: (FromJSON (AesonV log err a), Applicative (AesonV log err)) =>
  H.HashMap T.Text Value
  -> T.Text
  -> Parser (AesonV log err (Maybe a))
obj .::? key = case H.lookup key obj of
                  Nothing -> pure $ pure Nothing
                  Just a  -> appendJsonKeyP (fmap Just <$> parseJSON a) key
{-# INLINE (.::?) #-}

------ # Sequencing
withArraySeqV :: (FromJSON (AesonV log err a), Semigroup err) =>
  (AesonV log err a -> Int -> AesonV log err a)
  -> Value
  -> AT.Parser (AesonV log err [a])
withArraySeqV f = withArray "AesonV [a]" parse
  where parse = fmap (sequenceRC f) . mapM parseJSON . V.toList

instance (Semigroup err, FromJSON (AesonV log err a)) => FromJSON (AesonV log err [a]) where
  parseJSON a = case a of
    (Array _) -> withArraySeqV (\log i -> appendAesonV log (jsonIndex i)) a
    _         -> pure incorrectTypeError

appendAesonV :: (Semigroup outerLog) =>
  ComposeHV outerLog outerError innerLog innerError a
  -> outerLog
  -> ComposeHV outerLog outerError innerLog innerError a
appendAesonV x log = composeHV $ ((unComposeHV x) *<> log)

---- ********************************************************************************
---- Parser
---- ********************************************************************************

localP :: (outerLog -> outerLog)
          -> AT.Parser (ComposeHV outerLog outerError innerLog innerError a)
          -> AT.Parser (ComposeHV outerLog outerError innerLog innerError a)
localP f = fmap (localC f)

--appendP :: Semigroup log =>
--           Parser (ReaderAccValidation log err a)
--           -> log
--           -> Parser (ReaderAccValidation log err a)
appendP a log = localP (<> log) a
