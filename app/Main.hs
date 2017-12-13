module Main where

import System.IO (readFile)
import Data.Sequence as S (fromList)
import Lib
import qualified Advent1 as A1 (answer1, answer2)
import qualified Advent2 as A2 (answer1, answer2)
import qualified Advent3 as A3 (answer1, answer2)
import qualified Advent4 as A4 (answer1, answer2)
import qualified Advent5 as A5 (answer1, answer2)
import qualified Advent6 as A6 (answer1, answer2)
import Advent7 (buildTree, root, unbalanced)
import Advent8 (process, advent8)
import Advent9 (scoreStream, countGarbage)
import Advent10 
import qualified Advent12 as A12 (answer1, answer2)
import qualified Advent13 as A13 (answer1, answer2)

main :: IO ()
main = do 
  content1 <- readFile "data/advent1.txt"
  let ls1 = lines content1
  putStrLn $ "Advent 1-1: " ++ show (A1.answer1 ls1) 
  putStrLn $ "Advent 1-2: " ++ show (A1.answer2 ls1)
  
  content2 <- readFile "data/advent2.txt"
  let ls2 = lines content2
  putStrLn $ "Advent 2-1: " ++ show (A2.answer1 ls2) 
  putStrLn $ "Advent 2-2: " ++ show (A2.answer2 ls2)

  putStrLn $ "Advent 3-1: " ++ show (A3.answer1 361527)
  putStrLn $ "Advent 3-2: " ++ show (A3.answer2 361527)
  
  content4 <- readFile "data/advent4.txt"
  let ls4 = lines content4
  putStrLn $ "Advent 4-1: " ++ show (A4.answer1 ls4) 
  putStrLn $ "Advent 4-2: " ++ show (A4.answer2 ls4)
 
  content5 <- readFile "data/advent5.txt"
  let ls5 = lines content5
  putStrLn $ "Advent 5-1: " ++ show (A5.answer1 ls5)
  -- slow! 
  -- putStrLn $ "Advent 5-2: " ++ show (A5.answer2 ls5)
  
  content6 <- readFile "data/advent6.txt"
  let ls6 = lines content6
  -- slow!
  -- putStrLn $ "Advent 6-1: " ++ show (A6.answer1 ls6)
  -- putStrLn $ "Advent 6-2: " ++ show (A6.answer2 ls6)
  
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

  content13 <- readFile "data/advent13.txt"
  let ls13 = lines content13
  putStrLn $ "Advent 13-1: " ++ show (A13.answer1 ls13) 
  -- slow!
  -- putStrLn $ "Advent 13-2: " ++ show (A13.answer2 ls13)

