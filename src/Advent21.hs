module Advent21 where

import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as Ma (Map, empty, insert, lookup)
import Data.Matrix     as M (Matrix, matrix, ncols, (!), (<->), (<|>), nrows, fromList, toList, splitBlocks, joinBlocks, transpose, submatrix)
import Data.Maybe (fromJust)

import           Debug.Trace

instance Ord a => Ord (M.Matrix a) where
  a `compare` b = compare (toList a) (toList b)

buildMatrix :: [String] -> M.Matrix Char
buildMatrix xs =
  let dim = length xs
  in (M.fromList dim dim . concat) xs

t0 :: M.Matrix Char
t0 = buildMatrix [".#.", "..#", "###"]

answer1 :: [String] -> Int
answer1 = length . filter (== '#') . toList . enhance 5 . parse

answer2 :: [String] -> Int
answer2 xs = go 1 5 t0 $ parse xs
  where
    go mult 0 m rs = let (mult', m') = zoom m rs
                         n = (length . filter (== '#') . toList) m'
                     in trace (show n) n * mult * mult'
    go mult n m rs = let (mult', m') = zoom m rs 
                     in trace (show n ++ " " ++ show mult') go (mult * mult') (n - 1) m' rs

enhance :: Int -> Ma.Map (M.Matrix Char) (M.Matrix Char) -> M.Matrix Char
enhance n = go n t0
  where
    go 0 m rs = m
    go n m rs = go (n-1) (enhance' m rs) rs

enhance' :: M.Matrix Char -> Ma.Map (M.Matrix Char) (M.Matrix Char) -> M.Matrix Char
enhance' m rs
  | M.ncols m `mod` 2 == 0 = let dim = M.ncols m
                             in enhance2x2 m rs
  | M.ncols m `mod` 3 == 0 = let dim = M.ncols m
                             in enhance3x3 m rs

enhance2x2 :: M.Matrix Char -> Ma.Map (M.Matrix Char) (M.Matrix Char) -> M.Matrix Char
enhance2x2 m rs
  | M.ncols m == 2 && M.nrows m == 2 = trace (show m) fromJust $ Ma.lookup m rs
  | M.ncols m == 2                   = enhance2x2 (M.submatrix 1 2 1 2 m) rs <-> enhance2x2 (M.submatrix 3 (M.nrows m) 1 2 m) rs
  | M.nrows m == 2                   = enhance2x2 (M.submatrix 1 2 1 2 m) rs <|> enhance2x2 (M.submatrix 1 2 3 (M.ncols m) m) rs
  | otherwise                        = let (tl, tr, bl, br) = splitBlocks 2 2 m
                                       in joinBlocks (enhance2x2 tl rs, enhance2x2 tr rs, enhance2x2 bl rs, enhance2x2 br rs)

enhance3x3 :: M.Matrix Char -> Ma.Map (M.Matrix Char) (M.Matrix Char) -> M.Matrix Char
enhance3x3 m rs
  | M.ncols m == 3 && M.nrows m == 3 = trace (show m) fromJust $ Ma.lookup m rs
  | M.ncols m == 3                   = enhance3x3 (M.submatrix 1 3 1 3 m) rs <-> enhance3x3 (M.submatrix 4 (M.nrows m) 1 3 m) rs
  | M.nrows m == 3                   = enhance3x3 (M.submatrix 1 3 1 3 m) rs <|> enhance3x3 (M.submatrix 1 3 4 (M.ncols m) m) rs
  | otherwise                        = let (tl, tr, bl, br) = splitBlocks 3 3 m
                                       in joinBlocks (enhance3x3 tl rs, enhance3x3 tr rs, enhance3x3 bl rs, enhance3x3 br rs)

zoom :: M.Matrix Char -> Ma.Map (M.Matrix Char) (M.Matrix Char) -> (Int, M.Matrix Char)
zoom m rs
  | M.ncols m `mod` 2 == 0 = let multiplier = (M.ncols m `div` 2) ^ 2
                                 m' = enhance2x2 (M.submatrix 1 2 1 2 m) rs
                             in (multiplier, m')
  | M.ncols m `mod` 3 == 0 = let multiplier = (M.ncols m `div` 3) ^ 2
                                 m' = enhance3x3 (M.submatrix 1 3 1 3 m) rs
                             in (multiplier, m')

parse :: [String] -> Ma.Map (M.Matrix Char) (M.Matrix Char)
parse xs = let rules = map parseRule xs
           in foldr (\(k, v) m -> let rotations = take 4 $ iterate rotate k
                                      flips = map flipMatrix rotations
                                  in insert' (rotations ++ flips) v m) Ma.empty rules
  where
    insert' :: [M.Matrix Char] -> M.Matrix Char -> Ma.Map (M.Matrix Char) (M.Matrix Char) -> Ma.Map (M.Matrix Char) (M.Matrix Char) 
    insert' [] _ m = m
    insert' (k:ks) v m = insert' ks v (Ma.insert k v m)

rotate :: M.Matrix a -> M.Matrix a
rotate m = M.matrix (M.ncols m) (M.nrows m) (\(i, j) -> m ! (j, M.ncols m + 1 - i))

flipMatrix :: M.Matrix a -> M.Matrix a
flipMatrix m = M.matrix (M.ncols m) (M.nrows m) (\(i, j) -> m ! (M.ncols m + 1 - i, j))


-- transpose m = matrix (ncols m) (nrows m) $ \(i,j) -> m ! (j,i)0
--
parseRule :: String -> (M.Matrix Char, M.Matrix Char)
parseRule rs =
  let split = splitOn " => " rs
  in case split of
       [key, value] -> (buildMatrix $ splitOn "/" key, buildMatrix $ splitOn "/" value)

test :: [String]
test = ["../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#"]

