module Data.Validation.Reader
  ( ReaderAccValidation
  , module Data.Functor.Compose.Reader
  ) where

import Data.Functor.Compose.Reader
import Data.Validation

type ReaderAccValidation env err = ReaderC env (AccValidation err)

