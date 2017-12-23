module Advent21 where

import Data.List (sortBy, groupBy)
import Data.Ord
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as Ma (Map, empty, insert, lookup)
import Data.Matrix     as M (Matrix, matrix, ncols, (!), (<->), (<|>), nrows, fromList, toList, splitBlocks, joinBlocks, transpose, submatrix)
import Data.Maybe (fromJust)

import           Debug.Trace

instance Ord a => Ord (M.Matrix a) where
  a `compare` b = compare (toList a) (toList b)

type Rules = Ma.Map (M.Matrix Char) (M.Matrix Char)
type Grid  = M.Matrix Char

buildMatrix :: [String] -> M.Matrix Char
buildMatrix xs =
  let dim = length xs
  in (M.fromList dim dim . concat) xs

t0 :: M.Matrix Char
t0 = buildMatrix [".#.", "..#", "###"]

answer1 :: [String] -> Int
answer1 = countOn . enhance 5 . parse

answer2 :: [String] -> Int
answer2 = countOn . enhance 18 . parse

--answer2 :: [String] -> Int
--answer2 xs = let grid = [t0]
--                 gridsByOccurence = Ma.empty
--             in go 5 grid $ parse xs
--  where
--    go 0 m rs = let (mult', m') = zoom m rs
--                    n = (length . filter (== '#') . toList) m'
--                    in trace (show n) n * mult * mult'
--    go n grids rs = let (mult', m') = zoom m rs 
--                    in trace (show n ++ " " ++ show mult') go (mult * mult') (n - 1) m' rs

--enhance2 :: Int -> [(Int, Grid)] -> Rules -> Int
--enhance2 0 gs _  = foldl (\acc (m, g) -> (m * countOn g) + acc) 0 gs
--enhance2 n gs rs = let x9gs = map (\(m, g) -> (m, (flip enhance2x2 rs . flip enhance2x2 rs . flip enhance3x3 rs) g)) gs
--                       x3gs = concatMap (\(m, g) -> split3x3 m g) x9gs
--                       sorted = sortBy (comparing snd) x3gs
--                       grouped = groupBy (\a b -> snd a == snd b) sorted
--                   in trace (show grouped) $ undefined
--
--split3x3 :: Int -> Grid -> [(Int, Grid)]
--split3x3 m g = [(m, M.submatrix 1 3 1 3 g), (m, M.submatrix 1 3 4 6 g), (m, M.submatrix 1 3 7 9 g),
--                (m, M.submatrix 4 6 1 3 g), (m, M.submatrix 4 6 4 6 g), (m, M.submatrix 4 6 7 9 g),
--                (m, M.submatrix 7 9 1 3 g), (m, M.submatrix 7 9 4 6 g), (m, M.submatrix 7 9 7 9 g)]

countOn :: Grid -> Int
countOn = length . filter (== '#') . toList

enhance :: Int -> Rules -> M.Matrix Char
enhance n = go n t0
  where
    go 0 m rs = m
    go n m rs = let m' = enhance' m rs
                in trace (show n) $ m' `seq` go (n-1) m'  rs

enhance' :: M.Matrix Char -> Rules -> M.Matrix Char
enhance' m rs
  | M.ncols m `mod` 2 == 0 = trace ("2x2 " ++ show (M.ncols m)) $ enhance2x2 m rs
  | M.ncols m `mod` 3 == 0 = trace ("3x3 " ++ show (M.ncols m)) $ enhance3x3 m rs

enhance2x2 :: Grid -> Rules -> Grid 
enhance2x2 m rs
  | M.ncols m == 2 && M.nrows m == 2 = fromJust $ Ma.lookup m rs
  | M.ncols m == 2                   = let t = enhance2x2 (M.submatrix 1 2 1 2 m) rs
                                           b = enhance2x2 (M.submatrix 3 (M.nrows m) 1 2 m) rs
                                       in t `seq` b `seq` t <-> b
  | M.nrows m == 2                   = let l = enhance2x2 (M.submatrix 1 2 1 2 m) rs
                                           r = enhance2x2 (M.submatrix 1 2 3 (M.ncols m) m) rs
                                       in l `seq` r `seq` l <|> r
  | otherwise                        = let (tl, tr, bl, br) = splitBlocks 2 2 m
                                           tl' = enhance2x2 tl rs
                                           tr' = enhance2x2 tr rs
                                           bl' = enhance2x2 bl rs
                                           br' = enhance2x2 br rs
                                       in tl' `seq` tr' `seq` bl' `seq` br' `seq` joinBlocks (tl', tr', bl', br')

enhance3x3 :: Grid -> Rules -> Grid
enhance3x3 m rs
  | M.ncols m == 3 && M.nrows m == 3 = fromJust $ Ma.lookup m rs
  | M.ncols m == 3                   = let t = enhance3x3 (M.submatrix 1 3 1 3 m) rs
                                           b = enhance3x3 (M.submatrix 4 (M.nrows m) 1 3 m) rs
                                       in t `seq` b `seq` t <-> b
  | M.nrows m == 3                   = let l = enhance3x3 (M.submatrix 1 3 1 3 m) rs 
                                           r = enhance3x3 (M.submatrix 1 3 4 (M.ncols m) m) rs
                                       in l `seq` r `seq` l <|> r
  | otherwise                        = let (tl, tr, bl, br) = splitBlocks 3 3 m
                                           tl' = enhance3x3 tl rs
                                           tr' = enhance3x3 tr rs
                                           bl' = enhance3x3 bl rs
                                           br' = enhance3x3 br rs
                                       in tl' `seq` tr' `seq` bl' `seq` br' `seq` joinBlocks (tl', tr', bl', br')

zoom :: M.Matrix Char -> Rules -> (Int, M.Matrix Char)
zoom m rs
  | M.ncols m `mod` 2 == 0 = let multiplier = (M.ncols m `div` 2) ^ 2
                                 m' = enhance2x2 (M.submatrix 1 2 1 2 m) rs
                             in (multiplier, m')
  | M.ncols m `mod` 3 == 0 = let multiplier = (M.ncols m `div` 3) ^ 2
                                 m' = enhance3x3 (M.submatrix 1 3 1 3 m) rs
                             in (multiplier, m')

parse :: [String] -> Rules
parse xs = let rules = map parseRule xs
           in foldr (\(k, v) m -> let rotations = take 4 $ iterate rotate k
                                      flips = map flipMatrix rotations
                                  in insert' (rotations ++ flips) v m) Ma.empty rules
  where
    insert' :: [Grid] -> Grid -> Rules -> Rules
    insert' [] _ m = m
    insert' (k:ks) v m = insert' ks v (Ma.insert k v m)

rotate :: M.Matrix a -> M.Matrix a
rotate m = M.matrix (M.ncols m) (M.nrows m) (\(i, j) -> m ! (j, M.ncols m + 1 - i))

flipMatrix :: M.Matrix a -> M.Matrix a
flipMatrix m = M.matrix (M.ncols m) (M.nrows m) (\(i, j) -> m ! (M.ncols m + 1 - i, j))

parseRule :: String -> (Grid, Grid)
parseRule rs =
  let split = splitOn " => " rs
  in case split of
       [key, value] -> (buildMatrix $ splitOn "/" key, buildMatrix $ splitOn "/" value)

test :: [String]
test = ["../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#"]

