module Test (
  getByteString
, addSurvived
) where

import Person
import Data.Csv
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as B (encode)
import qualified Data.Vector as V (Vector, head, tail, concat, cons)

getByteString content = decode HasHeader content :: Either String (V.Vector (V.Vector L.ByteString))

addSurvived :: Int -> V.Vector L.ByteString -> V.Vector L.ByteString
addSurvived s bs = V.cons (V.head bs) (V.cons t (V.tail bs))
    where t = B.encode (s :: Int)
