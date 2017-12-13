module Advent13
    ( answer1
    , answer2
    ) where

import           Data.List.Split (splitOn)

answer1 :: [String] -> Int
answer1 xs = totalPenalty $ fillEmptyLayers 0 $ parse xs

answer2 :: [String] -> Int
answer2 xs = let init = fillEmptyLayers 0 $ parse xs
                 initByDelay = zip (iterate cycleScanners init) [0, 1..]
             in  snd $ head $ dropWhile fst $ map (\(s, i) -> (isCaught s, i))  initByDelay
test :: [String]
test =
  [ "0: 3"
  , "1: 2"
  , "4: 4"
  , "6: 4"]

data Layer = MkLayer { layer :: Int
                     , range :: Range
                     } deriving Show

data Range = Scanner Int Int Int
           | Empty
           deriving Show

mkLayer :: Int -> Int -> Layer
mkLayer l r = MkLayer l (Scanner (r - 1) 0 1)

parse :: [String] -> [Layer]
parse = map (parseLayers . split . removeWhitespace)

parseLayers :: [String] -> Layer
parseLayers [l, r] = mkLayer (toInt l) (toInt r)
--parseLayers xs = trace (show xs) $ mkLayer 1 1

fillEmptyLayers :: Int -> [Layer] -> [Layer]
fillEmptyLayers n [] = []
fillEmptyLayers n (x:xs) = if (layer x == n) then x : (fillEmptyLayers (n + 1) xs) else ((MkLayer n Empty) : (fillEmptyLayers (n + 1)  (x : xs)))

toInt :: String -> Int
toInt x = read x :: Int


split :: String -> [String]
split = splitOn ":"

removeWhitespace :: String -> String
removeWhitespace = filter (/= ' ')

cycleScanners :: [Layer] -> [Layer]
cycleScanners = map cycleScanner

cycleScanner :: Layer -> Layer
cycleScanner (MkLayer l r) = MkLayer l (nextRange r)

nextRange :: Range -> Range
nextRange (Scanner s p  1) = if s == p then Scanner s (p - 1) (-1) else Scanner s (p + 1) 1
nextRange (Scanner s p (-1)) = if p == 0 then Scanner s (p + 1) 1 else Scanner s (p - 1) (-1)
nextRange Empty = Empty

isCaught :: [Layer] -> Bool
isCaught []     = False
isCaught (x:xs) = if caught x then True else isCaught (cycleScanners xs)

totalPenalty :: [Layer] -> Int
totalPenalty = totalPenalty' 0
  where
    totalPenalty' :: Int -> [Layer] -> Int
    totalPenalty' p [] = p
    totalPenalty' p (x:xs) = if caught x then totalPenalty' (p + penalty x) (cycleScanners xs) else totalPenalty' p (cycleScanners xs)
    penalty :: Layer -> Int
    penalty (MkLayer l (Scanner s _ _)) = if l == 0 then 1 else l * (s + 1)
    penalty (MkLayer _ Empty)           = 0

caught :: Layer -> Bool
caught (MkLayer l (Scanner _ p _)) =  p == 0
caught (MkLayer l Empty)           = False
