{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Control.Lens
import qualified Data.ByteString as BS
import Data.Aeson
import Data.Validation
import Data.Text as T
import Data.Traversable as TR
import Data.Validation.Historical
import qualified Data.HashMap.Strict as H

-- Base validation
data VError = MustNotBeEmpty [String]
            | MustNotBeLongerThan32Characters [String]
            | Unknown
            deriving (Eq, Show)

type V = HistoricalV [String] [VError]

newtype Text32 = Text32 { unText32 :: Text } deriving (Eq, Show)

--string32 :: String -> V String32
--string32 t = reader f
--  where f c
--          | null t         = _Failure # [MustNotBeEmpty c]
--          | length t > 32  = _Failure # [MustBeLessThan32Length c t]
--          | otherwise      = _Success # String32 t

mkText32 :: Text -> V Text32
mkText32 t = reader f
  where f c
          | T.length t == 0 = _Failure # [MustNotBeEmpty c]
          | T.length t > 32 = _Failure # [MustNotBeLongerThan32Characters c]
          | otherwise       = _Success # Text32 t

-- Models
data Parent = Parent { parentName     :: Text32
                     , parentChild    :: Child
                     -- , parentChildren :: [Child]
                     } deriving (Eq, Show)

data Child = Child { childName :: Text32 } deriving (Eq, Show)

-- Smart constructors
mkParent :: V Text32 -> V Child -> V Parent
mkParent n c = Parent
               <$> n *<> ["_name"]
               <*> c *<> ["_child"]

mkChild :: V Text32 -> V Child
mkChild n = Child <$> n *<> ["_name"]


-- Nested?

data ParserError = KeyNotFound [ParserLog]
                 | IncorrectType [ParserLog]
                 deriving (Eq, Show)

data ParserLog = ParserLog T.Text
               | DomainLog String
               deriving (Eq, Show)

data NestedError = NEParser { unParser :: ParserError }
                 | NEDomain { unDomain :: VError }
                 deriving (Eq, Show)

unnest :: NestedError -> VError
unnest (NEDomain err) = err
unnest _              = error "unnest error"

type ParserV = HistoricalV [ParserLog] [NestedError]

-- Aeson

--(.::?) :: (FromJSON (AesonV log err a), Applicative (AesonV log err)) =>
--  H.HashMap T.Text Value
--  -> T.Text
--  -> Parser (AesonV log err (Maybe a))
--obj .::? key = case H.lookup key obj of
--                  Nothing -> pure $ pure Nothing
--                  Just a  -> appendJsonKeyP (fmap Just <$> parseJSON a) key
--{-# INLINE (.::?) #-}

--obj .:: key = appendJsonKeyP (obj .:? key .!= keyNotFoundError) key

--obj .::? key = case H.lookup key obj of
--                 Nothing -> pure $ pure
--                 Just a -> undefined


--appendJsonKeyP a key = appendP a $ jsonKey key

keyNotFoundError :: ParserV a
keyNotFoundError = reader $ \log -> _Failure # [NEParser (KeyNotFound log)]

obj .:: key = fmap (*<> [ParserLog key]) (obj .:? key .!= keyNotFoundError)


instance FromJSON (ParserV Text32) where
  parseJSON = withText "ParserV Text32" (pure . toParserV . mkText32)

instance FromJSON (ParserV Child) where
  parseJSON = withObject "ParserV Child" $ \o -> mkChild' <$> o .:: "name"

instance FromJSON (ParserV Parent) where
  parseJSON = withObject "ParserV Parent" $ \o -> mkParent' <$> o .:: "name" <*> o .:: "child"


--instance FromJSON (ParserV Child) where
--  parseJSON = withObject "ParserV Child" $ \o -> mkChild' <$> o .: "name"


-- (V Text32 -> V Child) -> (ParserV Text32 -> ParserV Child)

mkChild' :: ParserV Text32 -> ParserV Child
mkChild' n = toParserV $ mkChild (fromParserV n)

mkParent' :: ParserV Text32 -> ParserV Child -> ParserV Parent
mkParent' n c = toParserV $ mkParent (fromParserV n) (fromParserV c)

-- Reader log (AccValidation err a)





-- Test

testChild :: V Child
testChild = mkChild $ mkText32 "Test"


testChild' :: ParserV Child
testChild' = toParserV testChild

toParserV :: V a -> ParserV a
toParserV = reader . fmap (bimap (fmap NEDomain) (id)) . runReader

fromParserV :: ParserV a -> V a
fromParserV = reader . fmap (bimap (fmap unnest) (id)) . runReader

-- main
main :: IO ()
main = do
  _ <- printTest "Child Success:" "childsuccess.json" :: IO (Either String (ParserV Child))
  _ <- printTest "Child Failure:" "childfailure.json" :: IO (Either String (ParserV Child))
  _ <- printTest "Parent Success:" "parentsuccess.json" :: IO (Either String (ParserV Parent))
  _ <- printTest "Parent Failure:" "parentfailure.json" :: IO (Either String (ParserV Parent))
  _ <- printTest "Parent Failure 2:" "parentfailure2.json" :: IO (Either String (ParserV Parent))
  putStrLn "Done"

  --printTest "Parent Failure 2:" "parentfailure2.json" :: IO (Either String (ParserV Parent))


printTest :: (FromJSON (ParserV a), Show a) => T.Text -> String -> IO (Either String (ParserV a))
printTest title filename = do
  jsonData <- BS.readFile filename
  let v = eitherDecodeStrict' jsonData
  putStrLn ""
  putStrLn $ T.unpack title
  printJsonResult v
  return v


printJsonResult :: (Show a) => Either String (ParserV a) -> IO ()
printJsonResult (Left x) = putStrLn x
printJsonResult (Right x) = print $ (runReader x) []