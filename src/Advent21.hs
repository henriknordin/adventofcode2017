module Advent21
    ( parseInput
    , answer1
    , answer2
    ) where

import           Data.Maybe (fromJust)
import           Data.List (reverse, transpose)
import           Data.List.Split (splitOn, chunksOf)

import qualified Data.Map.Strict as Map (Map, empty, insert, lookup, size)


type Grid = [String]

parseInput :: String -> (Grid -> Grid)
parseInput xs = fromJust . flip Map.lookup rules
  where
    rules :: Map.Map Grid Grid
    rules = (parse . lines) xs

answer1 :: (Grid -> Grid) -> Int
answer1 rules = countOn $ iterate (zoom rules) start !! 5

answer2 :: (Grid -> Grid) -> Int
answer2 rules = countOn $ iterate (zoom rules) start !! 18

test :: [String]
test = ["../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#"]

start :: [String]
start = [".#.", "..#", "###"]

zoom :: (Grid -> Grid) -> Grid -> Grid
zoom rules xs =
  concatMap
    (map concat . transpose . map rules . transpose . map (chunksOf n))
    (chunksOf n xs)
  where
    n | even (length xs) = 2
      | otherwise = 3

parse :: [String] -> Map.Map Grid Grid
parse xs = 
  let rules = map parseRule xs
  in foldr (\(k, v) m -> let rotations = take 4 $ iterate rotate k
                             flips = take 4 $ iterate rotate $ reverse k
                         in insert (rotations ++ flips) v m) Map.empty rules
  where
    insert :: [Grid] -> Grid -> Map.Map Grid Grid -> Map.Map Grid Grid
    insert [] _ m = m
    insert (k:ks) v m = insert ks v (Map.insert k v m)

parseRule :: String -> (Grid, Grid)
parseRule rs =
  let [k, v] = splitOn " => " rs
  in (splitOn "/" k, splitOn "/" v)

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

countOn :: Grid -> Int
countOn = length . filter (== '#') . concat  

