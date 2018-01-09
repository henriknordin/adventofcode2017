module Main where

import System.IO (readFile)
import System.Environment

import qualified Advent01 as A01 (parseInput, captcha1, captcha2)
import qualified Advent02 as A02 (parseInput, checksum1, checksum2)
import qualified Advent03 as A03 (parseInput, manhattan, largerValue)
import qualified Advent04 as A04 (parseInput, passphrases1, passphrases2)
import qualified Advent05 as A05 (parseInput, steps1, steps2)
import qualified Advent06 as A06 (parseInput, dists)
import qualified Advent07 as A07 (parseInput, root, unbalanced)
import qualified Advent08 as A08 (parseInput, process, advent8)
import qualified Advent09 as A09 (parseInput, scoreStream, countGarbage)
import qualified Advent10 as A10 (parseInput, answer10_1, answer10_2)
import qualified Advent11 as A11 (parseInput, advent11_1, advent11_2)
import qualified Advent12 as A12 (parseInput, answer1, countGroups)
import qualified Advent13 as A13 (parseInput, answer1, answer2)
import qualified Advent14 as A14 (parseInput, answer1, answer2)
import qualified Advent15 as A15 (answer1, answer2)
import qualified Advent16 as A16 (parseInput, answer1, answer2)
import qualified Advent17 as A17 (parseInput, answer1, answer2)
import qualified Advent18 as A18 (parseInput, answer1, answer2)
import qualified Advent19 as A19 (answer1, answer2)
import qualified Advent20 as A20 (answer1, answer2)
import qualified Advent21 as A21 (answer1, answer2)
import qualified Advent22 as A22 (parseInput, answer1, answer2)
import qualified Advent23 as A23 (parseInput, answer1, answer2)
import qualified Advent24 as A24 (parseInput, answer1, answer2)
import qualified Advent25 as A25 (answer1)

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

advent07 :: IO ()
advent07 = do 
  input <- A07.parseInput <$> getInput 7
  putStrLn $ "Advent 7-1: " ++ show (A07.root input) -- svugo
  putStrLn $ "Advent 7-2: " ++ show (A07.unbalanced input 0) -- 1152

advent08 :: IO ()
advent08 = do 
  input <- A08.parseInput <$> getInput 8
  let (registers, maxValue) = A08.process input
  putStrLn $ "Advent 8-1: " ++ show (A08.advent8 registers) -- 5966
  putStrLn $ "Advent 8-2: " ++ show maxValue -- 6347

advent09 :: IO ()
advent09 = do 
  input <- A09.parseInput <$> getInput 9
  putStrLn $ "Advent 9-1: " ++ show (A09.scoreStream input) -- 15922
  putStrLn $ "Advent 9-2: " ++ show (A09.countGarbage input) -- 7314

advent10 :: IO ()
advent10 = do 
  input <- A10.parseInput <$> getInput 10
  putStrLn $ "Advent 10-1: " ++ show (A10.answer10_1 input) -- 8536 
  putStrLn $ "Advent 10-2: " ++ show (A10.answer10_2 input) -- aff593797989d665349efe11bb4fd99b

advent11 :: IO ()
advent11 = do 
  input <- A11.parseInput <$> getInput 11
  putStrLn $ "Advent 11-1: " ++ show (A11.advent11_1 input) -- 796
  putStrLn $ "Advent 11-2: " ++ show (A11.advent11_2 input) -- 1585

advent12 :: IO ()
advent12 = do 
  input <- A12.parseInput <$> getInput 12
  putStrLn $ "Advent 12-1: " ++ show (A12.answer1 input) -- 113
  putStrLn $ "Advent 12-2: " ++ show (A12.countGroups input) -- 202

advent13 :: IO ()
advent13 = do 
  input <- A13.parseInput <$> getInput 13
  putStrLn $ "Advent 13-1: " ++ show (A13.answer1 input) 
  putStrLn $ "Advent 13-2: " ++ show (A13.answer2 input)

advent14 :: IO ()
advent14 = do 
  input <- A14.parseInput <$> getInput 14
  putStrLn $ "Advent 14-1: " ++ show (A14.answer1 input) -- 8292
  putStrLn $ "Advent 14-2: " ++ show (A14.answer2 input) -- 1069

advent15 :: IO ()
advent15 = do 
  -- TODO Megaparsec
  putStrLn $ "Advent 15-1: " ++ show (A15.answer1 883 879) -- 609
  putStrLn $ "Advent 15-2: " ++ show (A15.answer2 883 879) -- 253

advent16 :: IO ()
advent16 = do 
  input <- A16.parseInput <$> getInput 16
  putStrLn $ "Advent 16-1: " ++ show (A16.answer1 input) -- ceijbfoamgkdnlph
  putStrLn $ "Advent 16-2: " ++ show (A16.answer2 input) -- pnhajoekigcbflmd

advent17 :: IO ()
advent17 = do 
  input <- A17.parseInput <$> getInput 17
  putStrLn $ "Advent 17-1: " ++ show (A17.answer1 input) -- 640
  putStrLn $ "Advent 17-2: " ++ show (A17.answer2 input) -- 47949463

advent18 :: IO ()
advent18 = do 
  input <- A18.parseInput <$> getInput 18
  putStrLn $ "Advent 18-1: " ++ show (A18.answer1 input) -- 3423
  putStrLn $ "Advent 18-2: " ++ show (A18.answer2 input) -- 7493

advent19 :: IO ()
advent19 = do 
  content <- readFile "data/input19.txt"
  let ls = lines content
  putStrLn $ "Advent 19-1: " ++ show (A19.answer1 ls) 
  putStrLn $ "Advent 19-2: " ++ show (A19.answer2 ls) 

advent20 :: IO ()
advent20 = do 
  content <- readFile "data/input20.txt"
  let ls = lines content
  putStrLn $ "Advent 20-1: " ++ show (A20.answer1 ls) 
  putStrLn $ "Advent 20-2: " ++ show (A20.answer2 ls) 

advent21 :: IO ()
advent21 = do 
  content <- readFile "data/input21.txt"
  let ls = lines content
  putStrLn $ "Advent 21-1: " ++ show (A21.answer1 ls) 
  putStrLn $ "Advent 21-2: " ++ show (A21.answer2 ls) 

advent22 :: IO ()
advent22 = do 
  input <- A22.parseInput <$> getInput 22
  putStrLn $ "Advent 22-1: " ++ show (A22.answer1 input 10000) 
  putStrLn $ "Advent 22-2: " ++ show (A22.answer2 input 10000000) 

advent23 :: IO ()
advent23 = do 
  input <- A23.parseInput <$> getInput 23
  putStrLn $ "Advent 23-1: " ++ show (A23.answer1 input) 
  --putStrLn $ "Advent 23-2: " ++ show (A23.answer2 ls) 

advent24 :: IO ()
advent24 = do 
  input <- A24.parseInput <$> getInput 24
  putStrLn $ "Advent 24-1: " ++ show (A24.answer1 input) -- 1906
  putStrLn $ "Advent 24-2: " ++ show (A24.answer2 input) -- 1824

advent25 :: IO ()
advent25 = 
  -- TODO Megaparsec
  putStrLn $ "Answer 1: " ++ show A25.answer1

parse :: [String] -> IO ()
parse ["01"] = advent01
parse ["02"] = advent02
parse ["03"] = advent03
parse ["04"] = advent04
parse ["05"] = advent05
parse ["06"] = advent06
parse ["07"] = advent07
parse ["08"] = advent08
parse ["09"] = advent09
parse ["10"] = advent10
parse ["11"] = advent11
parse ["12"] = advent12
parse ["13"] = advent13
parse ["14"] = advent14
parse ["15"] = advent15
parse ["16"] = advent16
parse ["17"] = advent17
parse ["18"] = advent18
parse ["19"] = advent19
parse ["20"] = advent20
parse ["21"] = advent21
parse ["22"] = advent22
parse ["23"] = advent23
parse ["24"] = advent24
parse ["25"] = advent25

main :: IO ()
main = getArgs >>= parse

