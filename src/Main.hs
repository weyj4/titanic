module Main where

import System.Environment (getArgs)
import System.IO (withFile, hGetContents, IOMode( ReadMode ), getLine)
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import Train (getData, splitGender, splitSurvived)

main :: IO ()
main = do
  putStrLn "Would you like to train or test?"
  command <- getLine
  if command == "train"
    then do
      putStrLn "Please enter an input file:"
      train
      else return ()

train :: IO ()
train = do
  inp <- getLine
  withFile inp ReadMode $ \dataHandle -> do
    content <- L.hGetContents dataHandle
    case getData content of
      (Left err) -> putStrLn err
      (Right dat) -> do
        let (males, females) = splitGender dat
        let (liveMales, deadMales) = splitSurvived males
        let (liveFemales, deadFemales) = splitSurvived females
        print $ (fromIntegral $ V.length liveMales) / (fromIntegral $ V.length males)
        print $ (fromIntegral $ V.length liveFemales) / (fromIntegral $ V.length females)

--test :: IO ()

