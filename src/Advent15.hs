{-# LANGUAGE NumDecimals #-}
module Advent15 
    ( parseInput
    , answer1
    , answer2
    ) where

import Data.Bits

parseInput :: String -> [Int]
parseInput = map (read . last . words) . lines

answer1 :: [Int] -> Int
answer1 xs = judge 40e6 generatorA generatorB
  where
    generatorA :: [Int]
    generatorA = generator 16807 1 $ head xs
    generatorB :: [Int]
    generatorB = generator 48271 1 $ last xs

answer2 :: [Int] -> Int
answer2 xs = judge  5e6 generatorA generatorB
  where
    generatorA :: [Int]
    generatorA = generator 16807 4 $ head xs
    generatorB :: [Int]
    generatorB = generator 48271 8 $ last xs

generator :: Int -> Int -> Int -> [Int]
generator f m s = let n = ((f * s) `mod` 2147483647) 
                  in if n `mod` m == 0 then n : generator f m n else generator f m n

pairs :: [Int] -> [Int] -> [(Int, Int)]
pairs = zip

judge :: Int -> [Int] -> [Int] -> Int
judge l a b = length $ filter (uncurry lowestBitsEqual) $ take l (pairs a b)

lowestBitsEqual :: Int -> Int -> Bool
lowestBitsEqual a b = let modulo = 2 ^ 16 - 1
                          a' = (.&.) a modulo
                          b' = (.&.) b modulo
                      in  a' == b' 
                      
