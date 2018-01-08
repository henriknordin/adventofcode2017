module Advent04
    ( parseInput
    , passphrases1
    , passphrases2
    ) where

import Data.List (sort, group)

parseInput :: String -> [String]
parseInput = lines

passphrases1 :: [String] -> Int
passphrases1 = length . filter (== True) . map valid1

passphrases2 :: [String] -> Int
passphrases2 = length . filter (== True) . map valid2

valid1 :: String -> Bool
valid1 = all (\x -> length x == 1) . group . sort . words 

valid2 :: String -> Bool
valid2 = all (\x -> length x == 1) . group . sort . map sort . words 

