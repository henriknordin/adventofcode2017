module Advent19
  ( answer1
  , answer2
  ) where

import           Data.List  (elemIndex)
import           Data.Maybe (fromMaybe)

test :: [String]
test =
  [ "    |         "
  , "    |  +--+   "
  , "    A  |  C   "
  , "F---|----E|--+"
  , "    |  |  |  D"
  , "    +B-+  +--+"
  ]

startX :: String -> Int
startX xs = fromMaybe (error "Could not find start position") (elemIndex '|' xs)

data Pos = Pos
  { x :: !Int
  , y :: !Int
  } deriving (Show)

data Der = Der
  { x' :: !Int
  , y' :: !Int
  } deriving (Show)

answer1 :: [String] -> String
answer1 xs =
  let startX' = startX $ head xs
  in filter (\x -> x /= '|' && x /= '+' && x /= '-') $ buildGraph (Pos startX' 0, Der 0 1) xs

answer2 :: [String] -> Int
answer2 xs =
  let startX' = startX $ head xs
  in 1 + length (buildGraph (Pos startX' 0, Der 0 1) xs)

mkDer :: Pos -> Pos -> Der
mkDer (Pos x y) (Pos x' y') = Der (x' - x) (y' - y)

buildGraph :: (Pos, Der) -> [String] -> String
buildGraph = go
  where
    go :: (Pos, Der) -> [String] -> String
    go (pos, der) chart =
      case next (pos, der) chart of
        Nothing                -> []
        Just (pos', der', val) -> val : go (pos', der') chart
    next :: (Pos, Der) -> [String] -> Maybe (Pos, Der, Char)
    next (pos, der) chart =
      let searchPos = nextLegal (findNext (pos, der)) chart
          searched = search searchPos chart
      in case searched of
           Nothing          -> Nothing
           Just (pos', val) -> Just (pos', mkDer pos pos', val)
    search :: [Pos] -> [String] -> Maybe (Pos, Char)
    search [] _ = Nothing
    search (Pos x y:ps) chart =
      let row = chart !! y
          val = row !! x
      in case val of
           ' ' -> search ps chart
           n   -> Just (Pos x y, n)

nextLegal :: [Pos] -> [String] -> [Pos]
nextLegal ps chart =
  filter (\(Pos x y) -> (x >= 0) && (x < length (head chart)) && (y >= 0) && (y < length chart)) ps

findNext :: (Pos, Der) -> [Pos]
findNext (Pos x y, Der x' y')
  | y' /= 0 = [Pos x (y + y'), Pos (x + 1) y, Pos (x - 1) y]
  | x' /= 0 = [Pos (x + x') y, Pos x (y + 1), Pos x (y - 1)]
  | otherwise = []
