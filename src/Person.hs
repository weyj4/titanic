{-# LANGUAGE DeriveGeneric #-}

module Person where

import Data.Csv
import Data.Text (Text, pack)
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

newtype DefaultSurvived = DefaultSurvived Int
  deriving Show

instance FromField DefaultSurvived where
    parseField s = case runParser (parseField s) of
      Left err -> pure $ DefaultSurvived 5
      Right n  -> pure $ DefaultSurvived n

