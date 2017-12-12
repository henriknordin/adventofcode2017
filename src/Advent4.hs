module Advent4
    ( answer1
    , answer2
    ) where

import Data.List (sort, group)

answer1 :: [String] -> Int
answer1 xs = length . filter (== True) $ map valid xs

answer2 :: [String] -> Int
answer2 xs = length . filter (== True) $ map valid2 xs

valid :: String -> Bool
valid xs = all (\x -> length x == 1) $ group $ sort $ words xs

valid2 :: String -> Bool
valid2 xs = all (\x -> length x == 1) $ group $ sort $ map sort $ words xs

