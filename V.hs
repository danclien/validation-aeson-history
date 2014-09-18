import Control.Applicative
import Data.Functor.Compose

type MaybeMaybe a = Compose Maybe Maybe a

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM x y = (+) <$> x <*> y

-- Using (+)
addMM :: MaybeMaybe Int -> MaybeMaybe Int -> MaybeMaybe Int
addMM x y = (+) <$> x <*> y

-- Using addM
addMM' :: MaybeMaybe Int -> MaybeMaybe Int -> MaybeMaybe Int
addMM' x y = Compose $ addM <$> (getCompose x) <*> (getCompose y)

--(Maybe Int -> Maybe Int) -> (Maybe (Maybe Int) -> Maybe (Maybe Int))


main = putStrLn "Done"
