{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Functor.Compose.Reader where

import           Control.Applicative
import qualified Control.Monad.Reader as R
import           Data.Functor.Compose
import           Data.Semigroup
import qualified Data.Traversable as TR

newtype ReaderC r f a =
  ReaderC { getReaderC :: Compose (R.Reader r) f a
          } deriving (Functor, Applicative)

-- Reader access
getReader :: ReaderC r f a -> R.Reader r (f a)
getReader = getCompose . getReaderC

liftReader :: R.Reader r (f a) -> ReaderC r f a
liftReader = ReaderC . Compose

-- Reader functions
singleton :: f a -> ReaderC r f a
singleton = liftReader . pure

local :: (r -> r)
         -> ReaderC r f a
         -> ReaderC r f a
local f a = liftReader $ R.local f (getReader a)

reader :: (r -> f a) -> ReaderC r f a
reader = liftReader . R.asks

runReader :: ReaderC r f a -> r -> f a
runReader x r = flip R.runReader r . getReader $ x

-- Reader Semigroup
(<>:) :: (Semigroup r) =>
  ReaderC r f a
  -> r
  -> ReaderC r f a
x <>: r = local (<> r) x
{-# INLINE (<>:) #-}

(>:) :: (Semigroup (r a), Applicative r) =>
  ReaderC (r a) f b
  -> a
  -> ReaderC (r a) f b
x >: a = x <>: pure a
{-# INLINE (>:) #-}
