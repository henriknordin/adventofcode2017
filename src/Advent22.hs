module Advent22 
  ( answer1
  , answer2)
  where

import Data.Map.Strict

type Position = (Int, Int)

data Direction = U | L | D | R deriving (Show, Enum)
data Carrier = Carrier Position Direction Int

data Cell = Clean | Weakened | Infected | Flagged deriving (Eq, Enum)

succDir :: Direction -> Direction
succDir R = U
succDir d = succ d

predDir :: Direction -> Direction
predDir U = R
predDir d = pred d

nextCell :: Cell -> Cell
nextCell Flagged = Clean
nextCell n = succ n

answer1 :: [String] -> Int -> Int
answer1 xs = burst (parse xs) (Carrier (0, 0) U 0)
  where 
    burst _ (Carrier _ _ i) 0 = i
    burst m c n = let (m', c') = move m c
                  in burst m' c' (n-1)

answer2 :: [String] -> Int -> Int
answer2 xs = burst (parse xs) (Carrier (0, 0) U 0)
  where 
    burst _ (Carrier _ _ i) 0 = i
    burst m c n = let (m', c') = moveEvolved m c
                  in burst m' c' (n-1) 



move :: Map Position Cell -> Carrier -> (Map Position Cell, Carrier)
move m (Carrier p d n) = let cell = findWithDefault Clean p m
                             d' = nextDir cell d
                             p' = next p d'
                             m' = if cell == Infected then delete p m else insert p Infected m
                             n' = if cell == Infected then n else n+1
                         in (m', Carrier p' d' n')
  where
    nextDir :: Cell -> Direction -> Direction
    nextDir Clean d = succDir d
    nextDir Infected d = predDir d
    
moveEvolved :: Map Position Cell -> Carrier -> (Map Position Cell, Carrier)
moveEvolved m (Carrier p d n) = 
  let cell = findWithDefault Clean p m
      d' = nextDir cell d
      p' = next p d'
      m' = if cell == Flagged then delete p m else insert p (nextCell cell) m
      n' = if nextCell cell == Infected then n+1 else n
  in (m', Carrier p' d' n')
  where
    nextDir :: Cell -> Direction -> Direction
    nextDir Clean d = succDir d
    nextDir Weakened d = d
    nextDir Infected d = predDir d
    nextDir Flagged d = (predDir . predDir) d

next :: Position -> Direction -> Position
next (x, y) U = (x, y+1)
next (x, y) D = (x, y-1)
next (x, y) L = (x-1, y)
next (x, y) R = (x+1, y)

test :: [String]
test = ["..#", "#..", "..."]

parse :: [String] -> Map Position Cell
parse xs = let offset = (length xs - 1) `div` 2
           in go empty offset offset xs
  where
    go :: Map Position Cell -> Int -> Int -> [String] -> Map Position Cell
    go m _ _ [] = m
    go m o y (x:xs) = go (parseLine m o y x) o (y-1) xs
    parseLine :: Map Position Cell -> Int -> Int -> String -> Map Position Cell
    parseLine m _ _ [] = m
    parseLine m o y (x:xs) = let m' = if x == '#' then insert (o - length xs, y) Infected m else m
                           in parseLine m' o y xs

