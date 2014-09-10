{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Validation.Aeson where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as H
import           Data.Semigroup
import qualified Data.Text as T
import qualified Data.Traversable as TR
import           Data.Validation
import           Data.Validation.Historical
import qualified Data.Vector as V


data AesonVEnv env  = JsonKey T.Text
                    | JsonIndex Int
                    | Env env
                    deriving (Eq, Show)

data AesonVError env err = AesonKeyNotFound   (V.Vector (AesonVEnv env))
                         | AesonIncorrectType (V.Vector (AesonVEnv env))
                         | ValidationError    (V.Vector (AesonVEnv env)) err
                         deriving (Eq, Show)

type AesonV env err a = AccValidationH 
                        (V.Vector (AesonVEnv env))
                        (V.Vector (AesonVError env err))
                        a

-- Default errors
incorrectType :: AesonV env err a
incorrectType = asksV $ \c -> _Failure # (V.singleton (AesonIncorrectType c))

missingKey :: AesonV env err a
missingKey = asksV $ \c -> _Failure # (V.singleton $ AesonKeyNotFound c)


withObjectV' :: a -> (Object -> AT.Parser a) -> Value -> AT.Parser a
withObjectV' err parse a = 
  case a of
    (Object o) -> parse o
    _          -> pure $ err

-- #

(.::?) :: (FromJSON (AccValidationH env err a), Semigroup err) => 
           Object 
           -> T.Text 
           -> AT.Parser (AccValidationH env err (Maybe a))
obj .::? key = case H.lookup key obj of
                  Nothing -> pure $ pure Nothing
                  Just a  -> fmap Just <$> parseJSON a
{-# INLINE (.::?) #-}

-- # Sequencing
withArraySeqV :: (Semigroup err, Semigroup env, FromJSON (AccValidationH env err a)) =>
                       (Int -> env) 
                       -> String
                       -> Value 
                       -> AT.Parser (AccValidationH env err [a])
withArraySeqV f s = withArray s parse
    where parse = fmap (sequenceV f) . mapM parseJSON . V.toList

sequenceV :: (Semigroup err, Semigroup env) =>
                   (Int -> env)
                   -> [AccValidationH env err a]
                   -> AccValidationH env err [a]
sequenceV f xs = TR.sequenceA xs''
  where g (va, i) = va .+ f i
                    --asksV $ \c -> runV va (c <> f i)
        xs'       = zip xs [0..]      
        xs''      = fmap g xs'

-- # Parsing Helpers (only for 1-3 parameter data constructors right now)

parseArray ::  (FromJSON (AccValidationH env err a), Semigroup err,
                     Semigroup env) =>
                    AccValidationH env err [a]
                    -> (Int -> env)
                    -> String
                    -> Value
                    -> AT.Parser (AccValidationH env err [a])
parseArray vIncorrectType f name a = case a of
  (Array _) -> withArraySeqV f name a
  _         -> pure vIncorrectType
