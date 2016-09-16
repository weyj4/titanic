module Main where

import System.Environment (getArgs)
import System.IO (withFile, hGetContents, IOMode( ReadMode ), getLine, IOMode (ReadWriteMode))
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import Person
import Train (getData, splitGender, splitSurvived)
import Test (getByteString, addSurvived)

main :: IO ()
main = do
  putStrLn "Would you like to train or test?"
  command <- getLine
  if command == "train"
    then do
      putStrLn "Please enter an input file:"
      train
      else do 
        putStrLn "Please enter an input file:"
        putColumn

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

putColumn :: IO ()
putColumn = do
  inp <- getLine
  withFile inp ReadWriteMode $ \dataHandle -> do
    content <- L.hGetContents dataHandle
    case getByteString content of
      (Left err) -> putStrLn err
      (Right dat) -> do
        print (addSurvived 0 $ V.head dat)

--test :: IO ()

