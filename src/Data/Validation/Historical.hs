{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Validation.Historical where

import Control.Applicative
import Control.Monad.Reader
import Data.Validation
import Data.Functor.Compose

-- # Data.Validation.Historical 

newtype AccValidationH env err a = 
  AccValidationH { getV :: Compose (Reader env) (AccValidation err) a 
                 } deriving (Functor, Applicative) 

getReader :: AccValidationH env err a -> Reader env (AccValidation err a)
getReader = getCompose . getV

runV :: AccValidationH env err a -> env -> AccValidation err a
runV a env = flip runReader env . getReader $ a

asksV :: (env -> AccValidation err a) -> AccValidationH env err a
asksV = liftReader . asks

liftV :: AccValidation err a -> AccValidationH env err a
liftV = liftReader . pure

liftReader :: Reader env (AccValidation err a) -> AccValidationH env err a
liftReader = AccValidationH . Compose
