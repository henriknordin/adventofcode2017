module Advent02
    ( parseInput
    , checksum1
    , checksum2
    ) where

parseInput :: String -> [[Int]]
parseInput xs = map (map (\x -> read x :: Int) . words) $ lines xs

checksum1 :: [[Int]] -> Int
checksum1 = sum . map (\xs -> maximum xs - minimum xs)

checksum2 :: [[Int]] -> Int
checksum2 xs = sum $ map evenDivisable xs
  where
    evenDivisable :: [Int] -> Int
    evenDivisable xs' = head [ x `div` y | x <- xs', y <- xs', x `mod` y == 0 && x /= y]

