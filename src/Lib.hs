module Lib
    ( input5
    , solve''
    , solve2''
    , input6
    , dists
    ) where

import Data.List (sort, group, unfoldr, isInfixOf, partition, find, sortBy)
import qualified Data.Sequence as S (Seq, index, length, update, elemIndexL, fromList)
import Data.Maybe (fromJust, isJust)

-- parse xs = map (map (\x -> read x :: Int)) $ map words xs
--input5 :: String
input5 = map (\x -> read x :: Int) $ words "0 3 0 1 -3"

solve'' :: Int -> Int -> S.Seq Int -> Int
solve'' n ind xs = let value = S.index xs ind
                   in if ind >= S.length xs then n else solve'' (n + 1) (ind + value) (S.update ind (value + 1) xs)

solve2'' :: Int -> Int -> S.Seq Int -> Int
solve2'' n ind xs = let value = S.index xs ind
                    in if ind >= S.length xs then n else solve2'' (n + 1) (ind + value) (S.update ind (if value < 3 then value + 1 else value - 1) xs) 
input6 :: S.Seq Int
input6 = S.fromList [14, 0, 15, 12, 11, 11, 3, 5, 1, 6, 8, 4, 9, 1, 8, 4]

dists :: S.Seq Int -> (S.Seq Int, Int)
dists xs = dists 1 [] xs
  where 
    dists cycles combs xs' = let next = redistribute xs'
                                 nextCombs = next : combs
                      in if next `elem` combs then (next, cycles) else dists (cycles +1) nextCombs next

-- advent 6-2: use input of advent 6-1 - 1 => 1037

redistribute :: S.Seq Int ->  S.Seq Int
redistribute xs = let blocks = maximum xs
                      ind    = fromJust $ S.elemIndexL blocks xs 
                  in redistribute' blocks ind (S.update ind 0 xs)
  where 
    redistribute' :: Int -> Int -> S.Seq Int -> S.Seq Int
    redistribute' blocks' ind' xs' = let nextInd = if ind' == length xs' - 1 then 0 else ind' + 1
                                         value = S.index xs' nextInd
                                       in if blocks' > 0 then redistribute' (blocks' - 1) nextInd (S.update nextInd (value + 1) xs') else xs'




