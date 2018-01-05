module Advent24
  ( answer1
  , answer2
  ) where

import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Foldable (toList)
import qualified Data.Sequence as S (Seq (..), fromList, elemIndexL, deleteAt, filter)
import Debug.Trace

answer1 :: [String] -> Int
answer1 xs = maximum $ map strength $ f 0 [] $ S.fromList $ parseAll xs

answer2 :: [String] -> Int
answer2 xs = 
  let bs = f 0 [] $ S.fromList $ parseAll xs
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

test' :: S.Seq Component
test' = S.fromList $ parseAll test

data Component = Component { a :: !Int
                           , b :: !Int
                           } deriving (Show, Eq)

--combinations xs = filter nub $ mapM (const xs) [1..length xs]
-- let it just take some tuple instead?
--

findMatching :: Int -> [Component] -> [Component] 
findMatching x = filter (\c -> a c == x || b c == x)

--bridges :: [Component] -> [[Component]]
--brigdes xs = undefined
--  where 
--    f :: ([Component], Int) -> [[Component]]
--    f cs n = let next = findMatching n
--             in map (\x -> x:cs) next
--  where
--    genBridges xss = (:) <$> xs <*> xss

--combinations :: [[Component]] -> [Component] -> [[Component]]
--combinations []     cs = let start = starts cs
--                         in combinations

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




--combinations :: Int -> [a] -> [[a]]
--combinations 0 lst = [[]]
--combinations n lst = do
--    (x:xs) <- tails lst
--    rest   <- combinations (n-1) xs
--    return $ x : rest

--starts :: [Component] -> [Component]
--starts = filter (\c -> a c == 0 || b c == 0)

parseAll :: [String] -> [Component]
parseAll = map parse

parse :: String -> Component
parse xs = let parts = splitOn "/" xs
           in Component (read (head parts) :: Int) (read (head $ tail parts) :: Int) 
