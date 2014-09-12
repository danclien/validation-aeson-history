module Data.Validation.Parser where

import Control.Monad.Reader
import Data.Aeson.Types
import Data.Functor.Compose
import Data.Semigroup

type ParserReader r = Compose Parser ((->) r)

getParser :: ParserReader r a -> Parser (r -> a)
getParser = getCompose

liftParser :: Parser (r -> a) -> ParserReader r a
liftParser = Compose

(<>::) :: (Semigroup r) =>
  ParserReader r a
  -> r
  -> ParserReader r a
x <>:: r = liftParser $ (fmap (local (<> r)) (getParser x))

--x <>:: r = local (<> r) x
{-# INLINE (<>::) #-}

