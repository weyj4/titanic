module Train 
( getData
, splitGender
, splitSurvived
) where

import Data.Csv
import Data.Text (Text, pack)
import qualified Data.Vector as V
import Person

getData content = decode HasHeader content :: Either String (V.Vector Person)

splitGender :: V.Vector Person -> (V.Vector Person, V.Vector Person)
splitGender = V.partition (\s -> sex s == (pack "male"))

splitSurvived :: V.Vector Person -> (V.Vector Person, V.Vector Person)
splitSurvived = V.partition (\s -> survived s == 1)

--splitByField :: (Eq b) => (Person -> b) -> b -> V.Vector Person -> (V.Vector Person, V.Vector Person)
--splitByField field value = V.partition (\s -> field s == value)
