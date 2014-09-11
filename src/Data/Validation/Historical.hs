module Data.Validation.Historical
  ( AccValidationH
  , module Data.Functor.Compose.Reader
  ) where

import Data.Functor.Compose.Reader
import Data.Validation

type AccValidationH env err = ReaderC env (AccValidation err)