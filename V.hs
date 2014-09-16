{-# LANGUAGE RankNTypes #-}

data Foo a b = Foo { listA :: [a]
                   , listB :: [b]
                   } deriving (Eq, Show)

type BarA a = forall b. (Num b, Show b) => Foo a b
type BarB b = forall a. Foo a b
type Bar = forall a b. Foo a b

appendA :: BarA Int -> BarA Int
appendA (Foo a b) = Foo (a ++ [42]) b

appendB :: BarB String -> BarB String
appendB (Foo a b) = Foo a (b ++ ["Test"])

appendA' :: Foo Int b -> Foo Int b
appendA' (Foo a b) = Foo (a ++ [42]) b

appendB' :: Foo a String -> Foo a String
appendB' (Foo a b) = Foo a (b ++ ["Test"])

test1 = appendA $ appendA $ Foo [32] [35]
test2 = appendB $ appendB $ Foo [] ["Test"]


main = do
  let input = Foo [0] [""] :: Foo Int String
  -- Doesn't compile:
  --print $ appendA $ appendA $ test1
  print $ appendB' $ appendA' $ input