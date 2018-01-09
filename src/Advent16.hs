module Advent16 
    ( parseInput
    , answer1
    , answer2
    ) where

import Data.List.Split (splitOn)
import qualified Data.Foldable as F (toList)
import qualified Data.Sequence as S
import Data.Maybe (fromJust)

import Data.Tuple (swap)

programs :: String
programs = "abcdefghijklmnop"

parseInput :: String -> String
parseInput = head . lines

answer1 :: String -> String
answer1 inp = let steps = splitOn "," inp
              in dance "abcdefghijklmnop" steps

-- pnhajoekigcbflmd
answer2 :: String -> String
answer2 inp = let instructions = splitOn "," inp
                  infIns = repeat instructions
                  infStates = danceSequence programs infIns
                  cycleLength = snd $ head $ dropWhile (\(state,i) -> state /= programs || i == 0) $ zip infStates [0,1..]
                  cycles = 1000000000 `mod` cycleLength
              in fst $ head $ dropWhile (\(_, i) -> i /= cycles) $ zip infStates [0,1..]

danceSequence :: String -> [[String]] -> [String]
danceSequence ps (s:steps) = ps : danceSequence (dance ps s) (tail steps)


dance :: String -> [String] -> String
dance = foldl dancestep
  where
    dancestep :: String -> String -> String
    --dancestep ps ('s':xs) = spin ps (toInt xs)
    dancestep ps ('s':xs) = rotateRight (toInt xs) ps
    dancestep ps ('x':xs) = let (a,b) = parseX xs
                            in exchange ps a b
    dancestep ps ('p':xs) = let (a, b) = parseP xs
                            in partner ps a b
parseX :: String -> (Int, Int)
parseX s = let a = takeWhile (/= '/') s
               b = drop 1 (dropWhile (/= '/') s)
           in (toInt a, toInt b)

parseP :: String -> (Char, Char)
parseP s = let a = head s
               b = head $ drop 1 (dropWhile (/= '/') s)
           in (a, b)

toInt :: String -> Int
toInt s = read s :: Int

rotateLeft :: Int -> [a] -> [a]
rotateLeft n = uncurry (++) . swap . splitAt n

rotateRight :: Int -> [a] -> [a]
rotateRight n xs = uncurry (++) . swap $ splitAt (length xs - n) xs

spin :: [a] -> Int -> [a] 
spin xs s = let (beg, end) = splitAt (length xs - s) xs
            in end ++ beg

exchange :: [a] -> Int -> Int -> [a]
exchange xs a b = let seq = S.fromList xs
                      va  = S.index seq a
                      vb  = S.index seq b
                      seq' = S.update a vb (S.update b va seq)
                  in F.toList seq' 

partner :: Eq a => [a] -> a -> a -> [a]
partner xs a b = let seq = S.fromList xs
                     ia  = fromJust $ S.elemIndexL a seq 
                     ib  = fromJust $ S.elemIndexL b seq
                     seq' = S.update ib a (S.update ia b seq)
                  in F.toList seq' 

