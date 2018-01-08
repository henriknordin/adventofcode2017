module Main (main) where

import Lib (getInput)

import Advent05 (parseInput, steps1, steps2)

main :: IO ()
main = do 
  input <- parseInput <$> getInput 5
  putStrLn $ "Advent 5-1: " ++ show (steps1 input)  -- 343364
  putStrLn $ "Advent 5-2: " ++ show (steps2 input)  -- 25071947
