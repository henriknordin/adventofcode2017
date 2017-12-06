module Main where

import System.IO (readFile)
import Data.Sequence as S (fromList)
import Lib

main :: IO ()
main = do 
  content <- readFile "/home/henrik/projects/adventofcode2017/data/advent4.txt"
  let ls = lines content
  let ans = length . filter (== True) $ map valid ls
  putStrLn $ "Advent 4-1: " ++ show ans
  let ans2 = length . filter (== True) $ map valid2 ls
  putStrLn $ "Advent 4-2: " ++ show ans2

  content5 <- readFile "/home/henrik/projects/adventofcode2017/data/advent5.txt"
  let ls5 = lines content5
  --putStrLn $ "Advent 5-1: " ++ solve' 0 (map (\x -> read x :: Int) $ words ls5)
  --putStrLn $ "Advent 5-1: " ++ show (solve' 0 0 (concat $ parse ls5))
  --putStrLn $ "Advent 5-2: " ++ show (solve2' 0 0 (concat $ parse ls5))
  putStrLn $ "Advent 5-1: " ++ show (solve'' 0 0 (S.fromList $ concat $ parse ls5))
  putStrLn $ "Advent 5-2: " ++ show (solve2'' 0 0 (S.fromList $ concat $ parse ls5))
