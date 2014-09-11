{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import           Data.Validation.Historical
import qualified Data.Vector as V

import           Models
import           Validation
import qualified Data.Functor.Compose.Reader as FRC

main :: IO ()
main = do
  _ <- printTest "Child Success:" "childsuccess.json" :: IO (Either String (V Child))
  -- AccSuccess (Child {childName = String32 "Sue"})

  _ <- printTest "Child Failure:" "childfailure.json" :: IO (Either String (V Child))
  -- AccFailure (fromList [ValidationError (fromList [Env "name",JsonKey "name"]) (MustNotBeEmpty "")])

  _ <- printTest "Parent Success:" "parentsuccess.json" :: IO (Either String (V Parent))
  -- AccSuccess (Parent {parentName = String32 "Parent Bob", parentChild = Just (Child {childName = String32 "Sue"}), parentChildren = [Child {childName = String32 "Sue"},Child {childName = String32 "Mary"},Child {childName = String32 "Joe"}]})

  _ <- printTest "Parent Failure:" "parentfailure.json" :: IO (Either String (V Parent))
  -- AccFailure (fromList [ValidationError (fromList [Env "name",JsonKey "name"]) (MustNotBeEmpty ""),ValidationError (fromList [Env "child",JsonKey "child",Env "name",JsonKey "name"]) (MustNotBeEmpty ""),ValidationError (fromList [Env "children",JsonKey "children",JsonIndex 1,Env "name",JsonKey "name"]) (MustNotBeEmpty "")])

  _ <- printTest "Parent Failure 2:" "parentfailure2.json" :: IO (Either String (V Parent))
  -- AccFailure (fromList [ValidationError (fromList [Env "name",JsonKey "name"]) (MustNotBeEmpty ""),AesonIncorrectType (fromList [Env "children",JsonKey "children",JsonIndex 0]),AesonKeyNotFound (fromList [Env "children",JsonKey "children",JsonIndex 1,Env "name",JsonKey "name"])])

  return ()

printTest :: (FromJSON (V a), Show a) => T.Text -> String -> IO (Either String (V a))
printTest title filename = do
  jsonData <- BS.readFile filename
  let v = eitherDecodeStrict' jsonData
  putStrLn ""
  putStrLn $ T.unpack title
  printJsonResult v
  return v

printJsonResult :: (Show a) => Either String (V a) -> IO ()
printJsonResult (Left x) = putStrLn x
printJsonResult (Right x) = print $ FRC.runReader x V.empty

