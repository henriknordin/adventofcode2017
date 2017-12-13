module Advent3 
    ( answer1
    , answer2

    , naturals
    , edges
    , manhattan
    , graph

    , cord
    , update

    ) where

import qualified Data.List as L (unfoldr)
import qualified Data.Matrix as M (Matrix, zero, getElem, setElem)

answer1 :: Int -> Int
answer1 = manhattan

answer2 :: Int -> Int
answer2 = largerValue

-- 1 2 3
-- 2 
-- 3


data Direction = R   
               | U  
               | L 
               | D 
               deriving (Show)

naturals :: [Int]
naturals = iterate (+1) 1

edges :: [Int]
edges = concatMap (replicate 2) [1, 2..]

graph :: [Direction]
graph = concat $ zipWith replicate  edges (cycle [R, U, L, D])   

manhattan n = (\(h, v) -> abs h + abs v) $ foldl step (0, 0) $ take (n - 1) graph

step :: (Int, Int) -> Direction -> (Int, Int)
step (h, v) R = (h + 1, v)
step (h, v) U = (h, v + 1)
step (h, v) L = (h - 1, v)
step (h, v) D = (h, v - 1)

cord :: [(Int, Int)]
cord = L.unfoldr (\d -> Just (foldl step (0, 0) $ take d graph, d + 1)) 0

largerValue :: Int -> Int
largerValue i = let m = M.zero 11 11 
                    m'' = M.setElem 1 (6, 6) m
                    offset  = (6, 6)
                in fst $ 
                   head $ 
                   dropWhile (\x -> fst x < i) $ 
                   L.unfoldr (\(n, m') -> let (n', mu) = update m' offset (cord !! n)
                                          in Just ((n', mu), (n + 1, mu))) (1, m'')

--        matrix          offset        cord    matrix
update :: M.Matrix Int -> (Int, Int) -> (Int, Int) -> (Int, M.Matrix Int)
update m (ox, oy) (dx, dy) = let x = ox + dx
                                 y = oy + dy
                                 v = sum [ M.getElem (x - 1) y m
                                         , M.getElem (x + 1) y m
                                         , M.getElem (x - 1) (y + 1) m
                                         , M.getElem x (y + 1) m
                                         , M.getElem (x + 1) (y + 1) m
                                         , M.getElem (x - 1) (y - 1) m
                                         , M.getElem x (y - 1) m
                                         , M.getElem (x + 1) (y - 1) m]
                             in (v, M.setElem v (x, y) m)
