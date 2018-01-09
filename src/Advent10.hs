module Advent10 
    ( parseInput
    , answer10_1
    , answer10_2

    -- needed by advent14
    , hexadecimal
    , sparseHash
    , denseHash
    , mkA
    , convert
    ) where

import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Data.List
import Data.List.Split (splitOn)
import Data.Bits (xor)
import Numeric (showHex)
import Data.Char (ord)

data A = MkA { ring :: ![Int]
             , index :: !Int
             , skip :: !Int
             } deriving Show

mkA :: Int -> A
mkA x = MkA [0, 1.. x] 0 0 

circularList :: [Int]
circularList = [0, 1, 2, 3, 4]

inputLengths :: [Int]
inputLengths = [3, 4, 1, 5]

parseInput :: String -> String
parseInput = head . lines

parseInts :: String -> [Int]
parseInts = map (\x -> read x :: Int) . splitOn ","

answer10_1 :: String -> Int
answer10_1 xs = product $ take 2 $ ring $ processAll (mkA 255) $ parseInts xs

answer10_2 :: String -> String
answer10_2 xs = hexadecimal $ denseHash $ sparseHash (mkA 255) $ convert xs

processAll :: A -> [Int] -> A
processAll = foldl process 

process :: A -> Int -> A
process a x = let ring' = ring a
                  index' = index a
                  skip' = skip a
                  sublist = map fst $ takeWhile (\a -> snd a < index' + x) $ dropWhile (\a -> snd a < index') $ zip (ring' ++ ring') [0, 1..]
                  sublist' = zip (reverse sublist) [index', (index' + 1)..]
                  ring'' = S.fromList ring'
                  newList = foldl (\acc a -> S.update (snd a `mod` length ring') (fst a) acc) ring'' sublist' 
                  nextIndex = (index' + x + skip') `mod` length ring' 
              in  MkA (F.toList newList) nextIndex (skip' + 1)

convert :: String -> [Int]
convert = map ord

sparseHash :: A -> [Int] -> A 
sparseHash a xs = let xs' = xs ++ suffix
                  in foldl (\a' _ -> processAll a' xs') a [1, 2.. 64] 
  where 
    suffix :: [Int]
    suffix = [17, 31, 73, 47, 23]

denseHash :: A -> [Int]
denseHash a = calculate $ ring a
  where
    calculate :: [Int] -> [Int]
    calculate [] = []
    calculate xs = foldl xor 0 (take 16 xs) : calculate (drop 16 xs)

hexadecimal :: [Int] -> String
hexadecimal xs = concatMap (\x -> if length x == 1 then "0" ++ x else x) $ foldl (\acc x -> acc ++ [showHex x ""]) [] xs
