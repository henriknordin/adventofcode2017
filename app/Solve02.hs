module Main (main) where

import Lib (getInput)

import Advent02 (parseInput, checksum1, checksum2)

main :: IO ()
main = do 
  input <- parseInput <$> getInput 2
  putStrLn $ "Advent 2-1: " ++ show (checksum1 input)  -- 37923
  putStrLn $ "Advent 2-2: " ++ show (checksum2 input)  -- 263
