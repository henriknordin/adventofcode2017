module Main where

import System.IO (readFile)
import Lib

main :: IO ()
main = do 
  content <- readFile "/home/henrik/projects/adventofcode2017/data/advent4.txt"
  let ls = lines content
  let ans = length . filter (== True) $ map valid ls
  putStrLn $ "Advent 4-1: " ++ show ans
  let ans2 = length . filter (== True) $ map valid2 ls
  putStrLn $ "Advent 4-2: " ++ show ans2

