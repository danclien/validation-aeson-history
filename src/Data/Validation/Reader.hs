module Data.Validation.Reader
  ( ReaderAccValidation
  , ReaderReaderAccValidation
  , module Data.Functor.Compose.Reader
  ) where

import Data.Functor.Compose.Reader
import Data.Validation

type ReaderAccValidation env err = ReaderC env (AccValidation err)

type ReaderReaderAccValidation out env err = ReaderC out (ReaderAccValidation env err)
