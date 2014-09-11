import Control.Applicative
import Control.Lens
import Data.Validation
import Data.Semigroup

data VError = VError Int deriving (Eq, Show)

data OuterError outer inner = OuterError outer
                            | InnerError inner
                            deriving (Eq, Show)


type NestedV outer inner a = AccValidation [OuterError outer inner] a

type V a = AccValidation VError a
type VOuter a = NestedV Int VError a

success :: V Int
success = _Success # 42

failure :: V Int
failure = _Failure # VError 0

successOuter :: VOuter Int
successOuter = _Success # 10

failureOuter :: VOuter Int
failureOuter = _Failure # [OuterError 0]


liftV :: V a -> VOuter a
liftV v = case v ^. _Either of
  Right a   -> pure a
  Left  err -> _Failure # [InnerError err]


main :: IO ()
main = do
  putStrLn $ "success:       " ++ show success
  putStrLn $ "failure:       " ++ show failure
  putStrLn $ "successOuter:  " ++ show successOuter
  putStrLn $ "failureOuter:  " ++ show failureOuter
  putStrLn $ "liftV success: " ++ show (liftV success)
  putStrLn $ "liftV failure: " ++ show (liftV failure)
  return ()


-- Reader env (AccValidation err a)

-- Reader parserEnv (Reader env (AccValidation err a))

-- Reader parserEnv (AccValdiation parserErr (Reader env (AccValidation err a)))

-- Reader parserEnv (AccValdiation parserErr (Reader env (AccValidation err a)))


-- Reader env (AccValidation err a) -> Reader (parserEnv, env) (AccValdiation (parserErr, err) a)


-- Reader (parserEnv, env) (AccValdiation (parserErr, err) a)




