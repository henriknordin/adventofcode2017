module Advent01
    ( parseInput
    , captcha1
    , captcha2
    ) where

import Data.Char (digitToInt)

parseInput :: String -> [Int]
parseInput = map digitToInt . head . lines

captcha1 :: [Int] -> Int
captcha1 xs = calculate (head xs) xs
  where
    calculate :: Int -> [Int] -> Int
    calculate a [x] = if a == x then x else 0
    calculate a (x:y:xs) = s + calculate a (y:xs) 
                           where s = if x == y then x else 0

captcha2 :: [Int] -> Int
captcha2 = calculate . splitHalf 
  where
    calculate :: ([Int], [Int]) -> Int
    calculate (xs, ys) = sum $ map (\(a, _) -> 2 * a) $ filter (uncurry (==)) $ zip xs ys

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt half xs
               where half = length xs `div` 2 

