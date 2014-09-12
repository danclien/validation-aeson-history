{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validation.Aeson where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as H
import           Data.Semigroup
import qualified Data.Text as T
import           Data.Validation
import           Data.Validation.Historical as H
import qualified Data.Vector as V

-- # Instances
instance Semigroup (V.Vector a) where
  (<>) = mappend

-- # Data types
data AesonVEnv env  = JsonKey T.Text
                    | JsonIndex Int
                    | Env env
                    deriving (Eq, Show)

data AesonVError env err = AesonKeyNotFound   (V.Vector (AesonVEnv env))
                         | AesonIncorrectType (V.Vector (AesonVEnv env))
                         | ValidationError    (V.Vector (AesonVEnv env)) err
                         deriving (Eq, Show)

type AesonV env err = AccValidationH
                      (V.Vector (AesonVEnv env))
                      (V.Vector (AesonVError env err))

-- # Default errors
incorrectType :: AesonV env err a
incorrectType = reader $ \c -> _Failure # pure (AesonIncorrectType c)

missingKey :: AesonV env err a
missingKey = reader $ \c -> _Failure # pure (AesonKeyNotFound c)

-- # Reader functions inside Parser

localP :: (env -> env)
          -> AT.Parser (AccValidationH env err a)
          -> AT.Parser (AccValidationH env err a)
localP f = fmap (local f)

appendP :: Semigroup env =>
             AT.Parser (AccValidationH env err a)
             -> env
             -> AT.Parser (AccValidationH env err a)
appendP a env = localP (<> env) a

appendJsonKeyP :: AT.Parser (AesonV env err a)
                  -> T.Text
                  -> AT.Parser (AesonV env err a)
appendJsonKeyP a key = appendP a $ jsonKey key

-- # Helpers
withObjectV :: Applicative f => (Object -> f (AesonV env err a)) -> Value -> f (AesonV env err a)
withObjectV parse a =
  case a of
    (Object o) -> parse o
    _          -> pure incorrectType

verror :: V.Vector (AesonVEnv env) -> err -> V.Vector (AesonVError env err)
verror env err = V.singleton $ ValidationError env err

jsonKey :: T.Text -> V.Vector (AesonVEnv a)
jsonKey a = V.singleton (JsonKey a)

jsonIndex :: Int -> V.Vector (AesonVEnv a)
jsonIndex a = V.singleton (JsonIndex a)

(>:) :: AesonV env err a -> env -> AesonV env err a
a >: env = a H.>: Env env
{-# INLINE (>:) #-}

-- # JSON parsing combinators
(.::) :: FromJSON (AesonV env err a) => Object -> T.Text -> AT.Parser (AesonV env err a)
obj .:: key = appendJsonKeyP (obj .:? key .!= missingKey) key
{-# INLINE (.::) #-}

(.::?) :: FromJSON (AesonV env err a) =>
            H.HashMap T.Text Value
            -> T.Text
            -> AT.Parser (AesonV env err (Maybe a))
obj .::? key = case H.lookup key obj of
                  Nothing -> pure $ pure Nothing
                  Just a  -> appendJsonKeyP (fmap Just <$> parseJSON a) key
{-# INLINE (.::?) #-}

-- # Sequencing

withArraySeqV :: (FromJSON t, Applicative f) =>
  (t -> Int -> f a)
  -> Value
  -> AT.Parser (f [a])
withArraySeqV f = withArray "V [a]" parse
  where parse = fmap (sequenceRC f) . mapM parseJSON . V.toList

-- # FromJSON instances
instance FromJSON (AesonV env err Int) where
  parseJSON (Number n) = (pure . pure . floor) n
  parseJSON _          = pure incorrectType

instance FromJSON (AesonV env err a) => FromJSON (AesonV env err [a]) where
  parseJSON a = case a of
    (Array _) -> withArraySeqV (\env i -> env <>: jsonIndex i) a
    _         -> pure incorrectType
