module Main (main) where

import Lib (getInput)

import Advent03 (parseInput, manhattan, largerValue)

main :: IO ()
main = do 
  input <- parseInput <$> getInput 3
  putStrLn $ "Advent 3-1: " ++ show (manhattan input)  -- 362
  putStrLn $ "Advent 3-2: " ++ show (largerValue input)  -- 361527
