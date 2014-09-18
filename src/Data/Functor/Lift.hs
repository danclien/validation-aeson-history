module Data.Functor.Lift
  ( liftA
  , liftA2
  , liftA3
  , liftA4
  , liftA5
  , liftA6
  , liftA7
  , liftA8
  , liftA9
  , liftA10
  , liftA11
  , liftA12
  , liftA13
  , liftA14
  , liftA15
  , liftA16
  ) where

import Control.Applicative

liftA4 f a1 a2 a3 a4 =
  f <$> a1 <*> a2 <*> a3 <*> a4

liftA5 f a1 a2 a3 a4 a5 =
  f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5

liftA6 f a1 a2 a3 a4 a5 a6=
  f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6

liftA7 f a1 a2 a3 a4 a5 a6 a7 =
  f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7

liftA8 f a1 a2 a3 a4 a5 a6 a7 a8 =
  f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8

liftA9 f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
  f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*>
        a9

liftA10 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
  f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*>
        a9 <*> a10

liftA11 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
  f <$> a1 <*> a2  <*> a3  <*> a4  <*> a5  <*> a6  <*> a7  <*> a8 <*>
        a9 <*> a10 <*> a11

liftA12 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
  f <$> a1 <*> a2  <*> a3  <*> a4  <*> a5  <*> a6  <*> a7  <*> a8 <*>
        a9 <*> a10 <*> a11 <*> a12

liftA13 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 =
  f <$> a1 <*> a2  <*> a3  <*> a4  <*> a5  <*> a6  <*> a7  <*> a8 <*>
        a9 <*> a10 <*> a11 <*> a12 <*> a13

liftA14 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 =
  f <$> a1 <*> a2  <*> a3  <*> a4  <*> a5  <*> a6  <*> a7  <*> a8 <*>
        a9 <*> a10 <*> a11 <*> a12 <*> a13 <*> a14

liftA15 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 =
  f <$> a1 <*> a2  <*> a3  <*> a4  <*> a5  <*> a6  <*> a7  <*> a8 <*>
        a9 <*> a10 <*> a11 <*> a12 <*> a13 <*> a14 <*> a15

liftA16 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 =
  f <$> a1 <*> a2  <*> a3  <*> a4  <*> a5  <*> a6  <*> a7  <*> a8 <*>
        a9 <*> a10 <*> a11 <*> a12 <*> a13 <*> a14 <*> a15 <*> a16
