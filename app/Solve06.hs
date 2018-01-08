module Main (main) where

import Lib (getInput)

import Advent06 (parseInput, dists)

main :: IO ()
main = do 
  input <- parseInput <$> getInput 6
  let (state, cycles) = dists input
  putStrLn $ "Advent 6-1: " ++ show cycles  -- 11137
  putStrLn $ "Advent 6-2: " ++ show (snd (dists state) - 1) -- 1037
