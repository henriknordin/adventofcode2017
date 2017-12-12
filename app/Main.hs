module Main where

import System.IO (readFile)
import Data.Sequence as S (fromList)
import Lib
import Advent7 (buildTree, root, unbalanced)
import Advent8 (process, advent8)
import Advent9 (scoreStream, countGarbage)
import Advent10 
import qualified Advent12 as A12 (answer1, answer2)

main :: IO ()
main = do 
  content <- readFile "data/advent4.txt"
  let ls = lines content
  let ans = length . filter (== True) $ map valid ls
  putStrLn $ "Advent 4-1: " ++ show ans
  let ans2 = length . filter (== True) $ map valid2 ls
  putStrLn $ "Advent 4-2: " ++ show ans2

  content5 <- readFile "data/advent5.txt"
  let ls5 = lines content5
  --putStrLn $ "Advent 5-1: " ++ solve' 0 (map (\x -> read x :: Int) $ words ls5)
  --putStrLn $ "Advent 5-1: " ++ show (solve' 0 0 (concat $ parse ls5))
  --putStrLn $ "Advent 5-2: " ++ show (solve2' 0 0 (concat $ parse ls5))
  --putStrLn $ "Advent 5-1: " ++ show (solve'' 0 0 (S.fromList $ concat $ parse ls5))
  --putStrLn $ "Advent 5-2: " ++ show (solve2'' 0 0 (S.fromList $ concat $ parse ls5))

  content7 <- readFile "data/advent7.txt"
  let ls7 = lines content7
  let tower = buildTree ls7

  putStrLn $ "Advent 7-1: " ++ show (root tower)
  putStrLn $ "Advent 7-2: " ++ show (unbalanced tower 0)

  content8 <- readFile "data/advent8.txt"
  let ls8 = lines content8
  --putStrLn $ "Advent 8-1: " ++ show (advent8 $ fst $ process ls8)
  --putStrLn $ "Advent 8-2: " ++ show (snd $ process ls8)
  
  content9 <- readFile "data/advent9.txt"
  let ls9 = lines content9
  putStrLn $ "Advent 9-1: " ++ show (scoreStream $ head ls9 )
  putStrLn $ "Advent 9-2: " ++ show (countGarbage $ head ls9)

  putStrLn $ "Advent 10-1: " ++ show answer10_1
  putStrLn $ "Advent 10-2: " ++ show answer10_2

  content12 <- readFile "data/advent12.txt"
  let ls12 = lines content12
  putStrLn $ "Advent 12-1: " ++ show (A12.answer1 ls12) 
  putStrLn $ "Advent 12-2: " ++ show (A12.answer2 ls12)
