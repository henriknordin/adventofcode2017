--{-# LANGUAGE DuplicateRecordFields #-} 
module Advent17 
    ( parseInput
    , answer1
    , answer2
    ) where

import qualified Data.Sequence as S (Seq, fromList, singleton, insertAt, index, dropWhileL)

data Buffer = MkBuffer { buffer :: S.Seq Int
                       , index  :: !Int
                       } deriving Show

parseInput :: String -> Int
parseInput = (\x -> read x :: Int) . head . lines

mkBuffer :: Buffer
mkBuffer = MkBuffer (S.singleton 0) 0

answer1 :: Int -> Int
answer1 s = S.index (S.dropWhileL (/= 2017) $ buffer $ process s) 1

answer2 :: Int -> Int
answer2 s = process2 mkNoBuffer 0 s 1

--process :: Int -> Buffer
--process size = foldl (\b a -> step b size a) (mkBuffer 2017) [1,2..2017]

process :: Int -> Buffer
process size = foldr (\a b -> step b size a) mkBuffer (reverse [1,2..2017])


process2 :: NoBuffer -> Int -> Int -> Int -> Int
process2 tz ind sz 50000000 = nextValue $ snd $ step' tz ind sz 50000000
process2 tz ind sz        n = let (ind', tz') = step' tz ind sz n
                              in  process2 tz' ind' sz (n+1)

process2' :: NoBuffer -> Int -> Int -> Int -> Int
process2' tz ind sz n
  | n <= 50000000 = let (ind', tz') = step' tz ind sz n
                              in  process2 tz' ind' sz (n+1)
  | otherwise     = nextValue tz

--process2' tz ind sz 50000000 = nextValue $ snd $ step' tz ind sz 50000000
--process2' tz ind sz        n = 

step :: Buffer -> Int -> Int -> Buffer
step b n v = let ind = (index b + n) `mod` v
                 b' = S.insertAt (ind + 1) v (buffer b)
           in MkBuffer b' (ind + 1) 

data NoBuffer = MkNoBuffer   { index' :: !Int
                             , zeroIndex :: !Int
                             , nextValue :: !Int
                             } deriving Show

mkNoBuffer = MkNoBuffer 0 0 0

step' :: NoBuffer -> Int -> Int -> Int -> (Int, NoBuffer) 
step' tz ind sz v = let ind' = (ind + sz) `mod` v
                        tz' = evalTracker tz ind' v
                    in (ind' + 1, tz')
                
evalTracker :: NoBuffer -> Int -> Int -> NoBuffer
evalTracker tz ind v
  | ind < index' tz = MkNoBuffer 0 (zeroIndex tz + 1) (nextValue tz)
  | ind == index' tz = MkNoBuffer 0 (zeroIndex tz) v
  | otherwise       = tz

