{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Validation.Historical
  ( HistoricalV
  , module Data.Functor.Compose.Reader
  ) where

import Control.Applicative
import Control.Lens
import Data.Functor.Compose.Reader
import Data.Semigroup
import Data.Validation
import Data.Aeson.Types as AT
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

type HistoricalV log err = ReaderC log (AccValidation err)