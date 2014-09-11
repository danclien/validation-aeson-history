{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Validation.Historical
  ( AccValidationH(..)
  , asksV
  , runV
  , liftV
  , localV
  , (.+)
  , sequenceV
  ) where

import Control.Applicative
import Control.Monad.Reader
import Data.Functor.Compose

import Data.Semigroup
import Data.Validation
import Data.Traversable as TR

newtype AccValidationH env err a =
  AccValidationH { getV :: Compose (Reader env) (AccValidation err) a
                 } deriving (Functor, Applicative)

-- Reader access
getReader :: AccValidationH env err a -> Reader env (AccValidation err a)
getReader = getCompose . getV

liftReader :: Reader env (AccValidation err a) -> AccValidationH env err a
liftReader = AccValidationH . Compose


-- Other
runV :: AccValidationH env err a -> env -> AccValidation err a
runV a env = flip runReader env . getReader $ a

liftV :: AccValidation err a -> AccValidationH env err a
liftV = liftReader . pure

asksV :: (env -> AccValidation err a) -> AccValidationH env err a
asksV = liftReader . asks

localV :: (env -> env) -> AccValidationH env err a -> AccValidationH env err a
localV f m = liftReader $ local f (getReader m)

(.+) :: (Semigroup env) => AccValidationH env err a -> env -> AccValidationH env err a
a .+ env = localV (<> env) a
{-# INLINE (.+) #-}

sequenceV :: (Semigroup err, Semigroup env) =>
               (Int -> env)
               -> [AccValidationH env err a]
               -> AccValidationH env err [a]
sequenceV f xs = TR.sequenceA xs''
  where g (va, i) = va .+ f i
        xs'       = zip xs [0..]
        xs''      = fmap g xs'