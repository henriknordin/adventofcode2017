module Advent1
    ( answer1
    , answer2
    , captcha
    , captcha2
    ) where

import Data.Char (digitToInt)

answer1 :: [String] -> Int
answer1 = captcha . head

answer2 :: [String] -> Int
answer2 = captcha2 . head

captcha :: String -> Int
captcha xs = calculate (digitToInt $ head xs) (map digitToInt xs)
  where
    calculate :: Int -> [Int] -> Int
    calculate a [x] = if a == x then x else 0
    calculate a (x:y:xs) = s + calculate a (y:xs) 
                           where s = if x == y then x else 0

captcha2 :: String -> Int
captcha2 xs = calculate . splitHalf $ map digitToInt xs
  where
    calculate :: ([Int], [Int]) -> Int
    calculate (xs, ys) = sum $ map (\(a, _) -> 2 * a) $ filter (uncurry (==)) $ zip xs ys
                     
splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt half xs
               where half = length xs `div` 2 

