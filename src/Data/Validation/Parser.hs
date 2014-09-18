{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Validation.Parser where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Data.Aeson.Types
import Data.Functor.Compose
import Data.Semigroup
import Data.Validation
import Data.Validation.Compose
import Data.Validation.Historical as VH
import qualified Data.Text as T
import qualified Data.Vector as V

-- Internal data

data Log a b = Log { parserLog :: [a]
                   , log :: [b]
                   } deriving (Eq, Show)

data Error log a b = InternalError log a
                   | UserError log b
                   deriving (Eq, Show)

instance Semigroup (Log a b) where
  (Log il l) <> (Log il' l') = Log (il <> il') (l <> l')

instance Monoid (Log a b) where
  mempty = Log mempty mempty
  mappend (Log il l) (Log il' l') = Log (mappend il il') (mappend l l')

-- Type
type ParserV parserLog parserErr log err =
  ComposeHV
  (Log parserLog log)
  [Error (Log parserLog log) parserErr err]
  log
  err

internalError :: parsorError -> ParserV parserLog parsorError log err a
internalError error = ComposeHV $ Compose $ VH.reader $ \hLog -> _Failure # [InternalError hLog error]

