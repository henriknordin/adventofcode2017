module Advent24
  ( parseInput
  , answer1
  , answer2
  ) where

import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Foldable (toList)
import qualified Data.Sequence as S (Seq (..), fromList, elemIndexL, deleteAt, filter)
import Debug.Trace

parseInput :: String -> [Component]
parseInput = map parse . lines

answer1 :: [Component] -> Int
answer1 xs = maximum $ map strength $ f 0 [] $ S.fromList xs

answer2 :: [Component] -> Int
answer2 xs = 
  let bs = f 0 [] $ S.fromList xs
      longest = length $ maximumBy (comparing length) bs
  in maximum $ map strength $ filter (\x -> length x == longest) bs

test :: [String]
test = 
  [ "0/2"
  , "2/2"
  , "2/3"
  , "3/4"
  , "3/5"
  , "0/1"
  , "10/1"
  , "9/10"]

data Component = Component { a :: !Int
                           , b :: !Int
                           } deriving (Show, Eq)

findMatching :: Int -> [Component] -> [Component] 
findMatching x = filter (\c -> a c == x || b c == x)

strength :: [Component] -> Int
strength = foldr (\c s -> a c + b c + s) 0

f :: Int -> [Component] -> S.Seq Component -> [[Component]]
f n bs cs = 
  let xs = findMatching n (toList cs)
  in case xs of
    []  -> []
    --xs' -> map (\x -> x ++ (f 0 (S.deleteAt (fromJust $ S.elemIndexL x cs)))) xs'
    xs' -> map (\x -> bs ++ x) $ concatMap (\x -> g n x cs) xs' 

g :: Int -> Component -> S.Seq Component -> [[Component]]
g n x cs = 
  let cs' = S.deleteAt (fromJust $ S.elemIndexL x cs) cs 
      next = if a x  == n then b x else a x
  in [x] : f next [x] cs'

parse :: String -> Component
parse xs = let parts = splitOn "/" xs
           in Component (read (head parts) :: Int) (read (head $ tail parts) :: Int) 
