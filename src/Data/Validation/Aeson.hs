{-# LANGUAGE FlexibleContexts #-}

module Data.Validation.Aeson where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Aeson.Types as AT
import           Data.Semigroup
import qualified Data.Traversable as TR
import           Data.Validation.Historical
import qualified Data.Vector as V

-- # Sequencing
withArraySeqV :: (Semigroup err, Semigroup env, FromJSON (AccValidationH env err a)) =>
                       (Int -> env) 
                       -> String
                       -> Value 
                       -> AT.Parser (AccValidationH env err [a])
withArraySeqV f s = withArray s parse
    where parse = fmap (sequenceV f) . mapM parseJSON . V.toList

sequenceV :: (Semigroup err, Semigroup env) =>
                   (Int -> env)
                   -> [AccValidationH env err a]
                   -> AccValidationH env err [a]
sequenceV f xs = TR.sequenceA xs''
  where g (va, i) = asksV $ \c -> runV va (c <> f i)
        xs'       = zip xs [0..]      
        xs''      = fmap g xs'

-- # Parsing Helpers (only for 1-3 parameter data constructors right now)

parseArray ::  (FromJSON (AccValidationH env err a), Semigroup err,
                     Semigroup env) =>
                    AccValidationH env err [a]
                    -> (Int -> env)
                    -> String
                    -> Value
                    -> AT.Parser (AccValidationH env err [a])
parseArray vIncorrectType f name a = case a of
  (Array _) -> withArraySeqV f name a
  _         -> pure vIncorrectType


parseObject1 :: (Semigroup env, Applicative f) =>
                 AccValidationH env err a
                 -> (a1 -> a)
                 -> (Object -> (f (AccValidationH env err a1), env))
                 -> Value
                 -> f (AccValidationH env err a)
parseObject1 vIncorrectType mk envF a = 
  let parse (p0, env0) = validate <$> p0
        where validate i = asksV $ \c -> mk <$>
                                         runV i (c <> env0)
  in case a of 
    (Object o) -> parse $ envF o
    _          -> pure vIncorrectType

parseObject2 :: (Semigroup err, Semigroup env, Applicative f) =>
                 AccValidationH env err a
                 -> (a2 -> a1 -> a)
                 -> (Object
                     -> ((f (AccValidationH env err a2), env),
                         (f (AccValidationH env err a1), env)))
                 -> Value
                 -> f (AccValidationH env err a)
parseObject2 vIncorrectType mk envF a = 
  let parse ((p0, env0),
             (p1, env1)) = validate <$> p0 <*> p1
        where validate i j = asksV $ \c -> mk <$>
                                           runV i (c <> env0) <*>
                                           runV j (c <> env1)
  in case a of 
    (Object o) -> parse $ envF o
    _          -> pure vIncorrectType


parseObject3 :: (Semigroup err, Semigroup env, Applicative f) =>
                 AccValidationH env err a
                 -> (a3 -> a2 -> a1 -> a)
                 -> (Object
                     -> ((f (AccValidationH env err a3), env),
                         (f (AccValidationH env err a2), env),
                         (f (AccValidationH env err a1), env)))
                 -> Value
                 -> f (AccValidationH env err a)
parseObject3 vIncorrectType mk envF a = 
  let parse ((p0, env0),
             (p1, env1),
             (p2, env2)) = validate <$> p0 <*> p1 <*> p2
        where validate i j k = asksV $ \c -> mk <$>
                                             runV i (c <> env0) <*>
                                             runV j (c <> env1) <*>
                                             runV k (c <> env2)
  in case a of 
    (Object o) -> parse $ envF o
    _          -> pure vIncorrectType

