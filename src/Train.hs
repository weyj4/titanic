{-# LANGUAGE DeriveGeneric #-}

module Train 
( getData
, splitGender
, splitSurvived
) where

import Data.Csv
import Data.Text (Text, pack)
import qualified Data.Vector as V
import GHC.Generics

data Person = Person { passengerId :: !Int, survived :: !Int, pclass :: !Int
                     , name :: !Text, sex :: !Text, age :: !DefaultAge, sibSp :: !Int
                     , parch :: !Int, ticket :: !Text, fare :: !Float
                     , cabin :: !Text, embarked :: !Text
                     } deriving (Generic, Show)

instance FromRecord Person

newtype DefaultAge = DefaultAge Int
  deriving Show

instance FromField DefaultAge where
    parseField s = case runParser (parseField s) of
      Left err -> pure $ DefaultAge 35
      Right n  -> pure $ DefaultAge n
  
getData content = decode HasHeader content :: Either String (V.Vector Person)

splitGender :: V.Vector Person -> (V.Vector Person, V.Vector Person)
splitGender = V.partition (\s -> sex s == (pack "male"))

splitSurvived :: V.Vector Person -> (V.Vector Person, V.Vector Person)
splitSurvived = V.partition (\s -> survived s == 1)

--splitByField :: (Eq b) => (Person -> b) -> b -> V.Vector Person -> (V.Vector Person, V.Vector Person)
--splitByField field value = V.partition (\s -> field s == value)
