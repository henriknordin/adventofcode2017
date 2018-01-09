module Advent11 
    ( parseInput
    , advent11_1
    , advent11_2
    ) where

import Data.List.Split (splitOn)

test1 = "ne,ne,ne"
test2 = "ne,ne,sw,sw"
test3 = "ne,ne,s,s"
test4 = "se,sw,se,sw,sw"


parseInput :: String -> [String]
parseInput = splitOn "," . head . lines

walk :: [String] -> (Int, Int)
walk = foldl step (0, 0)

step :: (Int, Int) -> String -> (Int, Int)
step (x, y) d = let (x', y') = direction d
                in (x + x', y + y')

walk' :: [String] -> (Int, Int, Int)
walk' = foldl step' (0, 0, 0)

step' :: (Int, Int, Int) -> String -> (Int, Int, Int)
step' (x, y, m) d = let (x', y') = direction d
                        nextX = x + x'
                        nextY = y + y'
                        nextM = max m $ dist (nextX, nextY)
                    in (nextX, nextY, nextM)

-- NW|N |
-- --+--+--
-- SW|  |NE
-- --+--+--
--   |S |SE

direction :: String -> (Int, Int)
direction "nw" = (-1, 1)
direction "n" = (0, 1)
direction "sw" = (-1, 0)
direction "ne" = (1, 0)
direction "s" = (0, -1)
direction "se" = (1, -1)

dist :: (Int, Int) -> Int
dist (a, b) = maximum [abs a, abs b, abs (a + b)]

advent11_1 :: [String] -> Int
advent11_1 = dist . walk 

advent11_2 :: [String] -> Int
advent11_2 = (\(_, _, m) -> m) . walk'
