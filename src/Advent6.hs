module Advent6
    ( answer1
    , answer2
    , input6
    , dists
    ) where

import Data.List (sort, group, unfoldr, isInfixOf, partition, find, sortBy)
import qualified Data.Sequence as S (Seq, index, length, update, elemIndexL, fromList)
import Data.Maybe (fromJust, isJust)

answer1 :: [String] -> Int
answer1 xs = snd $ dists $ S.fromList $ parse $ head xs

answer2 :: [String] -> Int
answer2 xs = (snd $ dists $ fst $ dists $ S.fromList $ parse $ head xs) - 1


parse :: String -> [Int]
parse xs = map (\x -> read x :: Int) $ words xs

--parse :: String -> S.Seq Int
--parse xs = 

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


