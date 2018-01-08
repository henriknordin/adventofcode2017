module Main (main) where

import Lib (getInput)

import Advent04 (parseInput, passphrases1, passphrases2)

main :: IO ()
main = do 
  input <- parseInput <$> getInput 4
  putStrLn $ "Advent 4-1: " ++ show (passphrases1 input)  -- 477
  putStrLn $ "Advent 4-2: " ++ show (passphrases2 input)  -- 167
