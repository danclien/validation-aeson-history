{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import           Data.Validation.Historical

import           Models
import           Validation

main :: IO ()
main = do
  printTest "Child Success:" "childsuccess.json"
  -- AccFailure [JsonKeyNotFound ["child"],JsonKeyNotFound ["children"]]

  printTest "Child Failure:" "childfailure.json"
  -- AccFailure [MustNotBeEmpty ["name"] "",JsonKeyNotFound ["child"],JsonKeyNotFound ["children"]]

  printTest "Parent Success:" "parentsuccess.json"
  -- AccSuccess (Parent {parentName = String32 "Parent Bob", parentChild = Child {childName = String32 "Sue"}, parentChildren = [Child {childName = String32 "Sue"},Child {childName = String32 "Mary"},Child {childName = String32 "Joe"}]})

  printTest "Parent Failure:" "parentfailure.json"
  -- AccFailure [MustNotBeEmpty ["name"] "",MustNotBeEmpty ["child","name"] "",MustNotBeEmpty ["children","1","name"] ""]

  printTest "Parent Failure 2:" "parentfailure2.json"
  -- AccFailure [MustNotBeEmpty ["name"] "",JsonKeyNotFound ["child"],JsonIncorrectValueType ["children","0"],JsonKeyNotFound ["children","1","name"]]


printTest :: T.Text -> String -> IO ()
printTest title filename = do
  jsonData <- BS.readFile filename
  putStrLn ""
  putStrLn $ T.unpack title
  printJsonResult (eitherDecodeStrict' jsonData :: Either String (V Parent))

printJsonResult :: (Show a) => Either String (V a) -> IO ()
printJsonResult (Left x) = putStrLn x
printJsonResult (Right x) = print $ runV x []

