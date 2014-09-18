{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Validation.Compose where

import Control.Applicative
import Data.Semigroup
import Data.Functor.Compose
import Data.Validation.Historical
import Data.Functor.Lift

newtype ComposeHV outerLog outerError innerLog innerError a =
  ComposeHV
  { getComposeHV ::
    Compose
    (HistoricalV outerLog outerError)
    (HistoricalV innerLog innerError)
    a
  } deriving (Functor, Applicative)

composeHV = ComposeHV . Compose

unComposeHV = getCompose . getComposeHV

liftInnerV :: (Semigroup outerError) =>
  (HistoricalV innerLog innerError a)
  -> ComposeHV outerLog outerError innerLog innerError a
liftInnerV = ComposeHV . Compose . pure

localC f m = ComposeHV $ Compose $ local f (getCompose $ (getComposeHV m))

liftC f a1 = composeHV $
  f <$> (unComposeHV a1)

liftC2 :: Semigroup outerError =>
  (HistoricalV innerLog1 innerError1 a1
   -> HistoricalV innerLog2 innerError2 a2
   -> HistoricalV innerLog innerError a
  )
  -> ComposeHV outerLog outerError innerLog1 innerError1 a1
  -> ComposeHV outerLog outerError innerLog2 innerError2 a2
  -> ComposeHV outerLog outerError innerLog innerError a
liftC2 f a1 a2 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)

liftC3 :: Semigroup outerError =>
  (HistoricalV innerLog1 innerError1 a1
   -> HistoricalV innerLog2 innerError2 a2
   -> HistoricalV innerLog3 innerError3 a3
   -> HistoricalV innerLog innerError a
  )
  -> ComposeHV outerLog outerError innerLog1 innerError1 a1
  -> ComposeHV outerLog outerError innerLog2 innerError2 a2
  -> ComposeHV outerLog outerError innerLog3 innerError3 a3
  -> ComposeHV outerLog outerError innerLog innerError a
liftC3 f a1 a2 a3 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)

liftC4 f a1 a2 a3 a4 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)

liftC5 f a1 a2 a3 a4 a5 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)
    <*> (unComposeHV a5)

liftC6 f a1 a2 a3 a4 a5 a6 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)
    <*> (unComposeHV a5)
    <*> (unComposeHV a6)

liftC7 f a1 a2 a3 a4 a5 a6 a7 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)
    <*> (unComposeHV a5)
    <*> (unComposeHV a6)
    <*> (unComposeHV a7)

liftC8 f a1 a2 a3 a4 a5 a6 a7 a8 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)
    <*> (unComposeHV a5)
    <*> (unComposeHV a6)
    <*> (unComposeHV a7)
    <*> (unComposeHV a8)

liftC9 f a1 a2 a3 a4 a5 a6 a7 a8 a9 = composeHV $
  f <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)
    <*> (unComposeHV a5)
    <*> (unComposeHV a6)
    <*> (unComposeHV a7)
    <*> (unComposeHV a8)
    <*> (unComposeHV a9)

liftC10 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)
    <*> (unComposeHV a5)
    <*> (unComposeHV a6)
    <*> (unComposeHV a7)
    <*> (unComposeHV a8)
    <*> (unComposeHV a9)
    <*> (unComposeHV a10)

liftC11 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)
    <*> (unComposeHV a5)
    <*> (unComposeHV a6)
    <*> (unComposeHV a7)
    <*> (unComposeHV a8)
    <*> (unComposeHV a9)
    <*> (unComposeHV a10)
    <*> (unComposeHV a11)

liftC12 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)
    <*> (unComposeHV a5)
    <*> (unComposeHV a6)
    <*> (unComposeHV a7)
    <*> (unComposeHV a8)
    <*> (unComposeHV a9)
    <*> (unComposeHV a10)
    <*> (unComposeHV a11)
    <*> (unComposeHV a12)

liftC13 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)
    <*> (unComposeHV a5)
    <*> (unComposeHV a6)
    <*> (unComposeHV a7)
    <*> (unComposeHV a8)
    <*> (unComposeHV a9)
    <*> (unComposeHV a10)
    <*> (unComposeHV a11)
    <*> (unComposeHV a12)
    <*> (unComposeHV a13)

liftC14 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)
    <*> (unComposeHV a5)
    <*> (unComposeHV a6)
    <*> (unComposeHV a7)
    <*> (unComposeHV a8)
    <*> (unComposeHV a9)
    <*> (unComposeHV a10)
    <*> (unComposeHV a11)
    <*> (unComposeHV a12)
    <*> (unComposeHV a13)
    <*> (unComposeHV a14)

liftC15 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)
    <*> (unComposeHV a5)
    <*> (unComposeHV a6)
    <*> (unComposeHV a7)
    <*> (unComposeHV a8)
    <*> (unComposeHV a9)
    <*> (unComposeHV a10)
    <*> (unComposeHV a11)
    <*> (unComposeHV a12)
    <*> (unComposeHV a13)
    <*> (unComposeHV a14)
    <*> (unComposeHV a15)

liftC16 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 = composeHV $
  f <$> (unComposeHV a1)
    <*> (unComposeHV a2)
    <*> (unComposeHV a3)
    <*> (unComposeHV a4)
    <*> (unComposeHV a5)
    <*> (unComposeHV a6)
    <*> (unComposeHV a7)
    <*> (unComposeHV a8)
    <*> (unComposeHV a9)
    <*> (unComposeHV a10)
    <*> (unComposeHV a11)
    <*> (unComposeHV a12)
    <*> (unComposeHV a13)
    <*> (unComposeHV a14)
    <*> (unComposeHV a15)
    <*> (unComposeHV a16)
