module Advent06
  ( parseInput
  , dists
  ) where

import           Data.List     (find, group, isInfixOf, partition, sort, sortBy,
                                unfoldr)
import           Data.Maybe    (fromJust, isJust)
import qualified Data.Sequence as S (Seq, elemIndexL, fromList, index, length,
                                     update)

parseInput :: String -> S.Seq Int
parseInput = S.fromList . parse . head . lines

parse :: String -> [Int]
parse = map (\x -> read x :: Int) . words

dists :: S.Seq Int -> (S.Seq Int, Int)
dists = go 1 []
  where
    go :: Int -> [S.Seq Int] -> S.Seq Int -> (S.Seq Int, Int)
    go cycles combs xs' =
      let next = redistribute xs'
          nextCombs = next : combs
      in if next `elem` combs
           then (next, cycles)
           else go (cycles + 1) nextCombs next

-- advent 6-2: use input of advent 6-1 - 1 => 1037
redistribute :: S.Seq Int -> S.Seq Int
redistribute xs =
  let blocks = maximum xs
      ind = fromJust $ S.elemIndexL blocks xs
  in redistribute' blocks ind (S.update ind 0 xs)
  where
    redistribute' :: Int -> Int -> S.Seq Int -> S.Seq Int
    redistribute' blocks' ind' xs' =
      let nextInd =
            if ind' == length xs' - 1
              then 0
              else ind' + 1
          value = S.index xs' nextInd
      in if blocks' > 0
           then redistribute' (blocks' - 1) nextInd (S.update nextInd (value + 1) xs')
           else xs'
