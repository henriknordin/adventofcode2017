module Advent3 
    ( answer1
    , answer2

    , naturals
    , edges
    , manhattan
    , graph

    ) where

answer1 = manhattan 361527

answer2 = undefined

-- 1 2 3
-- 2 
-- 3


data Spiral = Start
              | R   
              | U  
              | L 
              | D 
              deriving (Show)

naturals :: [Int]
naturals = iterate (+1) 1

edges :: [Int]
edges = concatMap (replicate 2) [1, 2..]

graph :: [Spiral]
graph = concat $ zipWith (\a b -> replicate a b) edges (cycle [R, U, L, D])   

manhattan n = (\(h, v) -> abs h + abs v) $ foldr step (0, 0) $ take (n - 1) graph
  where
    step R (h, v) = (h + 1, v)
    step U (h, v) = (h, v + 1)
    step L (h, v) = (h - 1, v)
    step D (h, v) = (h, v - 1)


