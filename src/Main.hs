{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS

import           Models
import           Validation

main :: IO ()
main = do
  putStrLn ""

  childSuccess <- BS.readFile "childsuccess.json"
  putStrLn "Child Success:"
  printJsonResult (eitherDecodeStrict' childSuccess :: Either String (V Child))
  -- AccSuccess (Child {childName = String32 "Sue"})

  putStrLn ""

  childFailure <- BS.readFile "childfailure.json"
  putStrLn "Child Failure:"
  printJsonResult (eitherDecodeStrict' childFailure :: Either String (V Child))
  -- AccFailure [MustNotBeEmpty {
  --   key: "name",
  --   source: "{\"name\":\"\",\"id\":\"childfailure\"}"
  -- } ""]

  putStrLn ""

  parentSuccess <- BS.readFile "parentsuccess.json"
  putStrLn "Parent Success:"
  printJsonResult (eitherDecodeStrict' parentSuccess :: Either String (V Parent))
  -- AccSuccess (Parent {parentName = String32 "Parent Bob", parentChild = Child {childName = String32 "Sue"}, parentChildren = [Child {childName = String32 "Sue"},Child {childName = String32 "Mary"},Child {childName = String32 "Joe"}]})

  putStrLn ""

  parentFailure <- BS.readFile "parentfailure.json"
  putStrLn "Parent Failure:"
  printJsonResult (eitherDecodeStrict' parentFailure :: Either String (V Parent))
  -- AccFailure [MustNotBeEmpty {
  --   key: "name",
  --   source: "{\"child\":{\"name\":\"\",\"id\":\"childfailure\"},\"children\":[{\"name\":\"Sue\",\"id\":\"child0\"},{\"name\":\"\",\"id\":\"child1\"},{\"name\":\"Joe\",\"id\":\"child2\"}],\"name\":\"\",\"id\":\"parentfailure\"}"
  -- } "",MustNotBeEmpty {
  --   key: "name",
  --   source: "{\"name\":\"\",\"id\":\"childfailure\"}"
  -- } "",MustNotBeEmpty {
  --   key: "name",
  --   source: "{\"name\":\"\",\"id\":\"child1\"}"
  -- } ""]
  
  putStrLn ""

  parentFailure2 <- BS.readFile "parentfailure2.json"
  putStrLn "Parent Failure 2:"
  printJsonResult (eitherDecodeStrict' parentFailure2 :: Either String (V Parent))

  
  putStrLn ""

printJsonResult :: (Show a) => Either String (V a) -> IO ()
printJsonResult (Left x) = putStrLn x
printJsonResult (Right x) = print $ x JsonNoError 
   



