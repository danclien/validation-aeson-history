module Data.Functor.Compose.Reader where

import           Control.Applicative
import qualified Control.Monad.Reader as R
import           Data.Functor.Compose
import           Data.Semigroup
import qualified Data.Traversable as TR

type ReaderC r f = Compose (R.Reader r) f

-- Reader access
getReader :: ReaderC r f a -> R.Reader r (f a)
getReader = getCompose

liftReader :: R.Reader r (f a) -> ReaderC r f a
liftReader = Compose

mapReader :: (R.Reader r (f a) -> R.Reader r (f a))
             -> ReaderC r f a
             -> ReaderC r f a
mapReader f = liftReader . f . getReader

-- Reader functions
singleton :: f a -> ReaderC r f a
singleton = liftReader . pure

local :: (r -> r)
         -> ReaderC r f a
         -> ReaderC r f a
local f = mapReader $ R.local f

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

-- Sequence
sequenceRC :: (Applicative f) =>
  (t -> Int -> f a)
  -> [t]
  -> f [a]
sequenceRC f xs = TR.sequenceA xs''
  where xs'       = zip xs [0..]
        xs''      = fmap g xs'
        g (va, i) = f va i
