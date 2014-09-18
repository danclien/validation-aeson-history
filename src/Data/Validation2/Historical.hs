{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Validation.Historical where

import Control.Applicative
import Control.Lens
import Data.Functor.Compose.Reader
import Data.Semigroup
import Data.Validation
import Data.Validation.Reader
import Data.Aeson.Types as AT
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H


-- Logging and error handling

data Log f g a b = Log { internalLog :: f a
                       , log :: g b
                       } deriving (Eq, Show)

data Error log a b = InternalError log a
                   | UserError log b
                   deriving (Eq, Show)

instance (Semigroup (f a), Semigroup (g b)) => Semigroup (Log f g a b) where
  (Log il l) <> (Log il' l') = Log (il <> il') (l <> l')

instance (Monoid (f a), Monoid (g b)) => Monoid (Log f g a b) where
  mempty = Log mempty mempty
  mappend (Log il l) (Log il' l') = Log (mappend il il') (mappend l l')

type HistoricalV f internalLog g log h internalError err a =
  ReaderAccValidation
  (Log f g internalLog log)
  (h (Error (Log f g internalLog log) internalError err))
  a

internalErrorH error = reader $ \hLog -> _Failure # (pure $ InternalError hLog error)
errorH hLog error = pure $ UserError hLog error

