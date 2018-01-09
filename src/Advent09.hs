module Advent09
    ( parseInput
    , scoreStream
    , countGarbage
    ) where

parseInput :: String -> String
parseInput = head . lines

input :: [String]
input =
  [ "{}"
  , "{{{}}}"
  , "{{},{}}"
  , "{{{},{},{{}}}}"
  , "{<a>,<a>,<a>,<a>}"
  , "{{<ab>},{<ab>},{<ab>},{<ab>}}"
  , "{{<!!>},{<!!>},{<!!>},{<!!>}}"
  , "{{<a!>},{<a!>},{<a!>},{<ab>}}"]

scoreStream :: String -> Int
scoreStream xs = sum $ score 0 $ filterStream xs

score :: Int -> String -> [Int]
score s [] = [] 
score s ('{' : xs) = (s + 1) : score (s + 1) xs 
score s ('}' : xs) = score (s - 1) xs 
score s (x : xs) = score s xs

filterStream :: String -> String 
filterStream = filterGarbage . filterExclamation

filterExclamation :: String -> String
filterExclamation [] = []
filterExclamation ('!' : xs) = filterExclamation $ tail xs
filterExclamation (x : xs) = x : filterExclamation xs

filterGarbage :: String -> String
filterGarbage [] = []
filterGarbage ('<' : xs) = filterGarbage $ tail $ dropWhile (/= '>') xs
filterGarbage (x : xs)   = x : filterGarbage xs

countGarbage :: String -> Int
countGarbage xs = let xs' = filterExclamation xs
                  in countGarbage' xs' 0
  where
    countGarbage' :: String -> Int -> Int
    countGarbage' [] n = n
    countGarbage' ('<' : xs) n = let garbage = takeWhile (/= '>') xs
                                 in countGarbage' (tail $ dropWhile (/= '>') xs) (n + length garbage)
    countGarbage' (_ : xs) n = countGarbage' xs n

