module Advent05
    ( parseInput
    , steps1 
    , steps2 
    ) where

import qualified Data.Sequence as S (Seq, index, length, update, elemIndexL, fromList)

parseInput :: String -> S.Seq Int
parseInput = S.fromList . map read . words

steps1:: S.Seq Int -> Int
steps1 = go 0 0
  where
    go :: Int -> Int -> S.Seq Int -> Int
    go n ind xs = 
      let value = S.index xs ind
          xs' = S.update ind (value + 1) xs
      in if ind >= S.length xs 
           then n 
           else go (n + 1) (ind + value) xs'

steps2 :: S.Seq Int -> Int
steps2 = go 0 0
  where
    go :: Int -> Int -> S.Seq Int -> Int
    go n ind xs = 
      let value = S.index xs ind
          xs' = S.update ind (if value < 3 then value + 1 else value - 1) xs
      in if ind >= S.length xs 
           then n 
           else go (n + 1) (ind + value) xs'
