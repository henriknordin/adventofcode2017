module Advent15 
    ( answer1
    , answer2
    ) where

import Data.Bits

answer1 :: Int -> Int -> Int
answer1 a b = judge 40000000 (generatorA1 a) (generatorB1 b)

answer2 :: Int -> Int -> Int
answer2 a b = judge  5000000 (generatorA2 a) (generatorB2 b)

generator :: Int -> Int -> Int -> [Int]
generator f m s = let n = ((f * s) `mod` 2147483647) 
                  in if n `mod` m == 0 then n : generator f m n else generator f m n

generatorA1 :: Int -> [Int]
generatorA1 = generator 16807 1

generatorB1 :: Int -> [Int]
generatorB1 = generator 48271 1

generatorA2 :: Int -> [Int]
generatorA2 = generator 16807 4

generatorB2 :: Int -> [Int]
generatorB2 = generator 48271 8

pairs :: [Int] -> [Int] -> [(Int, Int)]
pairs = zip

judge :: Int -> [Int] -> [Int] -> Int
judge l a b = length $ filter (uncurry lowestBitsEqual) $ take l (pairs a b)

lowestBitsEqual :: Int -> Int -> Bool
lowestBitsEqual a b = let modulo = 2 ^ 16 - 1
                          a' = (.&.) a modulo
                          b' = (.&.) b modulo
                      in  a' == b' 
                      
