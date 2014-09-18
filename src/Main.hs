{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import           Data.Functor.Lift
import           Data.Validation.Compose
import           Data.Validation.Historical
import           Data.Functor.Compose.Reader
import qualified Data.Vector as V
import Data.Monoid

import           Models
import           Validation

main :: IO ()
main = do
  _ <- printTest "Child Success:" "childsuccess.json" :: IO (Either String (VP Child))
  -- AccSuccess (Child {childName = String32 "Sue"})

  _ <- printTest "Child Failure:" "childfailure.json" :: IO (Either String (VP Child))
  -- AccFailure (fromList [ValidationError (fromList [Env "name",JsonKey "name"]) (MustNotBeEmpty "")])

  _ <- printTest "Parent Success:" "parentsuccess.json" :: IO (Either String (VP Parent))
  -- AccSuccess (Parent {parentName = String32 "Parent Bob", parentChild = Just (Child {childName = String32 "Sue"}), parentChildren = [Child {childName = String32 "Sue"},Child {childName = String32 "Mary"},Child {childName = String32 "Joe"}]})

  _ <- printTest "Parent Failure:" "parentfailure.json" :: IO (Either String (VP Parent))
  -- AccFailure (fromList [ValidationError (fromList [Env "name",JsonKey "name"]) (MustNotBeEmpty ""),ValidationError (fromList [Env "child",JsonKey "child",Env "name",JsonKey "name"]) (MustNotBeEmpty ""),ValidationError (fromList [Env "children",JsonKey "children",JsonIndex 1,Env "name",JsonKey "name"]) (MustNotBeEmpty "")])

  _ <- printTest "Parent Failure 2:" "parentfailure2.json" :: IO (Either String (VP Parent))
  -- AccFailure (fromList [ValidationError (fromList [Env "name",JsonKey "name"]) (MustNotBeEmpty ""),AesonIncorrectType (fromList [Env "children",JsonKey "children",JsonIndex 0]),AesonKeyNotFound (fromList [Env "children",JsonKey "children",JsonIndex 1,Env "name",JsonKey "name"])])

  putStrLn "Done"

printTest :: (FromJSON (VP a), Show a) => T.Text -> String -> IO (Either String (VP a))
printTest title filename = do
  jsonData <- BS.readFile filename
  let v = eitherDecodeStrict' jsonData
  putStrLn ""
  putStrLn $ T.unpack title
  printJsonResult v
  return v

printJsonResult :: (Show a) => Either String (VP a) -> IO ()
printJsonResult (Left x) = putStrLn x
printJsonResult (Right x) = print $ "Test"
-- printJsonResult (Right x) = print $ runReader x mempty



test a = (runReader (unComposeHV a) mempty)

