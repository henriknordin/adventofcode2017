module Advent10 where

import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Data.List
import Data.Bits (xor)
import Numeric (showHex)
import Data.Char (ord)

data A = MkA { ring :: [Int]
             , index :: Int
             , skip :: Int
             } deriving Show

mkA :: Int -> A
mkA x = MkA [0, 1.. x] 0 0 

circularList :: [Int]
circularList = [0, 1, 2, 3, 4]

inputLengths :: [Int]
inputLengths = [3, 4, 1, 5]

answer10_1 :: Int
answer10_1 = product $ take 2 $ ring $ processAll (mkA 255) [97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190]

answer10_2 :: String
answer10_2 = hexadecimal $ denseHash $ sparseHash (mkA 255) $ convert "97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190"

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
    calculate xs = (foldl (\b a -> b `xor` a) 0 $ take 16 xs) : calculate (drop 16 xs)

hexadecimal :: [Int] -> String
hexadecimal xs = concat $ map (\x -> if length x == 1 then "0" ++ x else x) $ foldl (\acc x -> acc ++ [showHex x ""]) [] xs
