module Main where

import System.IO (readFile)
import System.Environment

import qualified Advent01 as A01 (parseInput, captcha1, captcha2)
import qualified Advent02 as A02 (parseInput, checksum1, checksum2)
import qualified Advent03 as A03 (parseInput, manhattan, largerValue)
import qualified Advent04 as A04 (parseInput, passphrases1, passphrases2)
import qualified Advent05 as A05 (parseInput, steps1, steps2)
import qualified Advent06 as A06 (parseInput, dists)

import Advent7 (buildTree, root, unbalanced)
import Advent8 (process, advent8)
import Advent9 (scoreStream, countGarbage)
import Advent10 
import qualified Advent12 as A12 (answer1, answer2)
import qualified Advent13 as A13 (answer1, answer2)
import qualified Advent14 as A14 (answer1, answer2)
import qualified Advent15 as A15 (answer1, answer2)
import qualified Advent16 as A16 (answer1, answer2)
import qualified Advent17 as A17 (answer1, answer2)
import qualified Advent18 as A18 (answer1, answer2)
import qualified Advent19 as A19 (answer1, answer2)
import qualified Advent20 as A20 (answer1, answer2)
import qualified Advent21 as A21 (answer1, answer2)
import qualified Advent22 as A22 (answer1, answer2)
import qualified Advent23 as A23 
import qualified Advent24 as A24 (answer1, answer2)

import Lib (getInput)

advent01 :: IO ()
advent01 = do
  input <- A01.parseInput <$> getInput 1
  putStrLn $ "Advent 1-1: " ++ show (A01.captcha1 input)  -- 1223
  putStrLn $ "Advent 1-2: " ++ show (A01.captcha2 input)  -- 1284

advent02 :: IO ()
advent02 = do
  input <- A02.parseInput <$> getInput 2
  putStrLn $ "Advent 2-1: " ++ show (A02.checksum1 input)  -- 37923
  putStrLn $ "Advent 2-2: " ++ show (A02.checksum2 input)  -- 263

advent03 :: IO ()
advent03 = do
  input <- A03.parseInput <$> getInput 3
  putStrLn $ "Advent 3-1: " ++ show (A03.manhattan input)  -- 362
  putStrLn $ "Advent 3-2: " ++ show (A03.largerValue input)  -- 361527

advent04 :: IO ()
advent04 = do 
  input <- A04.parseInput <$> getInput 4
  putStrLn $ "Advent 4-1: " ++ show (A04.passphrases1 input)  -- 477
  putStrLn $ "Advent 4-2: " ++ show (A04.passphrases2 input)  -- 167

advent05 :: IO ()
advent05 = do 
  input <- A05.parseInput <$> getInput 5
  putStrLn $ "Advent 5-1: " ++ show (A05.steps1 input)  -- 343364
  putStrLn $ "Advent 5-2: " ++ show (A05.steps2 input)  -- 25071947

advent06 :: IO ()
advent06 = do 
  input <- A06.parseInput <$> getInput 6
  let (state, cycles) = A06.dists input
  putStrLn $ "Advent 6-1: " ++ show cycles  -- 11137
  putStrLn $ "Advent 6-2: " ++ show (snd (A06.dists state) - 1) -- 1037

parse ["01"] = advent01
parse ["02"] = advent02
parse ["03"] = advent03
parse ["04"] = advent04
parse ["05"] = advent05
parse ["06"] = advent06

main :: IO ()
main = do 
  args <- getArgs

  parse args

--  input <- A01.parseInput <$> getInput 1
--  putStrLn $ "Advent 1-1: " ++ show (A01.captcha1 input)  -- 1223
--  putStrLn $ "Advent 1-2: " ++ show (A01.captcha2 input)  -- 1284
  
--  content7 <- readFile "data/advent7.txt"
--  let ls7 = lines content7
--  let tower = buildTree ls7
--
--  putStrLn $ "Advent 7-1: " ++ show (root tower)
--  putStrLn $ "Advent 7-2: " ++ show (unbalanced tower 0)

--  content8 <- readFile "data/advent8.txt"
--  let ls8 = lines content8
--  --putStrLn $ "Advent 8-1: " ++ show (advent8 $ fst $ process ls8)
--  --putStrLn $ "Advent 8-2: " ++ show (snd $ process ls8)
--  
--  content9 <- readFile "data/advent9.txt"
--  let ls9 = lines content9
--  putStrLn $ "Advent 9-1: " ++ show (scoreStream $ head ls9 )
--  putStrLn $ "Advent 9-2: " ++ show (countGarbage $ head ls9)
--
--  putStrLn $ "Advent 10-1: " ++ show answer10_1
--  putStrLn $ "Advent 10-2: " ++ show answer10_2
--
--  content12 <- readFile "data/advent12.txt"
--  let ls12 = lines content12
--  putStrLn $ "Advent 12-1: " ++ show (A12.answer1 ls12) 
--  putStrLn $ "Advent 12-2: " ++ show (A12.answer2 ls12)
--
--  content13 <- readFile "data/advent13.txt"
--  let ls13 = lines content13
--  putStrLn $ "Advent 13-1: " ++ show (A13.answer1 ls13) 
--  -- slow!
--  -- putStrLn $ "Advent 13-2: " ++ show (A13.answer2 ls13)
--
--  --putStrLn $ "Advent 14-1: " ++ show (A14.answer1 "ugkiagan")
--  --putStrLn $ "Advent 14-2: " ++ show (A14.answer2 "ugkiagan")

--  putStrLn $ "Advent 15-1: " ++ show (A15.answer1 883 879)
--  putStrLn $ "Advent 15-2: " ++ show (A15.answer2 883 879)

--  content16 <- readFile "data/advent16.txt"
--  let ls16 = lines content16
--  putStrLn $ "Advent 16-1: " ++ show (A16.answer1 $ head ls16) 
--  putStrLn $ "Advent 16-2: " ++ show (A16.answer2 $ head ls16)

--  putStrLn $ "Advent 17-1: " ++ show (A17.answer1 349)
--  putStrLn $ "Advent 17-2: " ++ show (A17.answer2 349)
  
--  content18 <- readFile "data/advent18.txt"
--  let ls18 = lines content18
--  putStrLn $ "Advent 18-1: " ++ show (A18.answer1 ls18) 
--  putStrLn $ "Advent 18-2: " ++ show (A18.answer2 ls18)
  
--  content19 <- readFile "data/advent19.txt"
--  let ls19 = lines content19
--  putStrLn $ "Advent 19-1: " ++ show (A19.answer1 ls19) 
--  putStrLn $ "Advent 19-2: " ++ show (A19.answer2 ls19) 

--  content20 <- readFile "data/advent20.txt"
--  let ls20 = lines content20
--  putStrLn $ "Advent 20-1: " ++ show (A20.answer1 ls20) 
--  putStrLn $ "Advent 20-2: " ++ show (A20.answer2 ls20) 

--  content21 <- readFile "data/advent21.txt"
--  let ls21 = lines content21
--  putStrLn $ "Advent 21-1: " ++ show (A21.answer1 ls21) 
--  putStrLn $ "Advent 21-2: " ++ show (A21.answer2 ls21) 
  
--  content22 <- readFile "data/advent22.txt"
--  let ls22 = lines content22
--  putStrLn $ "Advent 22-1: " ++ show (A22.answer1 ls22 10000) 
--  putStrLn $ "Advent 22-2: " ++ show (A22.answer2 ls22 10000000) 
  
--  content23 <- readFile "data/advent23.txt"
--  let ls23 = lines content23
--  putStrLn $ "Advent 23-1: " ++ show (A23.answer1 ls23) 
--  putStrLn $ "Advent 23-2: " ++ show (A23.answer2 ls23) 

--  content24 <- readFile "data/advent24.txt"
--  let ls24 = lines content24
--  putStrLn $ "Advent 24-1: " ++ show (A24.answer1 ls24) 
--  putStrLn $ "Advent 24-2: " ++ show (A24.answer2 ls24) 
