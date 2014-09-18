{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Functor.Compose.Reader where

import           Control.Applicative
import           Data.Functor.Compose
import           Data.Semigroup
import qualified Data.Traversable as TR

newtype ReaderC r f a = ReaderC { getReaderC :: Compose ((->) r) f a
                                } deriving (Functor, Applicative)

-- Reader access
runReader :: ReaderC r f a -> r -> f a
runReader = getCompose . getReaderC

reader :: (r -> (f a)) -> ReaderC r f a
reader = ReaderC . Compose

modifyReader :: ((r -> (f a)) -> (r -> (f a)))
             -> ReaderC r f a
             -> ReaderC r f a
modifyReader f = reader . f . runReader

-- Reader functions
lift :: f a -> ReaderC r f a
lift = reader . pure

local :: (r -> r)
         -> ReaderC r f a
         -> ReaderC r f a
local f m = reader $ \r -> runReader m (f r)

-- Reader Semigroup
(*<>) :: (Semigroup r) =>
  ReaderC r f a
  -> r
  -> ReaderC r f a
x *<> r = local (<> r) x
{-# INLINE (*<>) #-}

-- Sequence
sequenceRC :: (Applicative f) =>
  (t -> Int -> f a)
  -> [t]
  -> f [a]
sequenceRC f xs = TR.sequenceA xs''
  where xs'       = zip xs [0..]
        xs''      = fmap g xs'
        g (va, i) = f va i
