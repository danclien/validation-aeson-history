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

main :: IO ()
main = do
  _ <- printTest "Child Success:" "childsuccess.json" :: IO (Either String (V Child))
  -- AccFailure [JsonKeyNotFound ["child"],JsonKeyNotFound ["children"]]

  _ <- printTest "Child Failure:" "childfailure.json" :: IO (Either String (V Child))
  -- AccFailure [MustNotBeEmpty ["name"] "",JsonKeyNotFound ["child"],JsonKeyNotFound ["children"]]

  _ <- printTest "Parent Success:" "parentsuccess.json" :: IO (Either String (V Parent))
  -- AccSuccess (Parent {parentName = String32 "Parent Bob", parentChild = Child {childName = String32 "Sue"}, parentChildren = [Child {childName = String32 "Sue"},Child {childName = String32 "Mary"},Child {childName = String32 "Joe"}]})

  _ <- printTest "Parent Failure:" "parentfailure.json" :: IO (Either String (V Parent))
  -- AccFailure [MustNotBeEmpty ["name"] "",MustNotBeEmpty ["child","name"] "",MustNotBeEmpty ["children","1","name"] ""]

  _ <- printTest "Parent Failure 2:" "parentfailure2.json" :: IO (Either String (V Parent))
  -- AccFailure [MustNotBeEmpty ["name"] "",JsonKeyNotFound ["child"],JsonIncorrectValueType ["children","0"],JsonKeyNotFound ["children","1","name"]]

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
printJsonResult (Right x) = print $ runV x V.empty

