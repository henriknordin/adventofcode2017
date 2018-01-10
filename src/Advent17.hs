module Advent17
  ( parseInput
  , answer1
  , answer2
  ) where

import qualified Data.Sequence as S (Seq, dropWhileL, fromList, index, insertAt, singleton)
import Debug.Trace

parseInput :: String -> Int
parseInput = (\x -> read x :: Int) . head . lines

data Buffer = MkBuffer
  { buffer :: S.Seq Int
  , index  :: !Int
  } deriving (Show)

data NoBuffer = MkNoBuffer
  { position  :: !Int
  , size      :: !Int
  , zeroAt    :: !Int
  , afterZero :: !Int
  } deriving (Show)

answer1 :: Int -> Int
answer1 s = S.index (S.dropWhileL (/= 2017) $ buffer $ process s) 1

answer2 :: Int -> Int
answer2 = process2 buf
  where
    buf = MkNoBuffer 0 1 0 0

process :: Int -> Buffer
process size = foldr (\a b -> step b size a) buf (reverse [1,2 .. 2017])
  where 
    buf = MkBuffer (S.singleton 0) 0

step :: Buffer -> Int -> Int -> Buffer
step b n v =
  let ind = (index b + n) `mod` v
      b' = S.insertAt (ind + 1) v (buffer b)
  in MkBuffer b' (ind + 1)

process2 :: NoBuffer -- ^ The current state of the ring buffer 
         -> Int      -- ^ The step size
         -> Int      -- ^ The value after zero
process2 buf step
  | size buf <= 50000000 =
      let buf' = step' buf step
      in process2 buf' step
  | otherwise = afterZero buf

step' :: NoBuffer  -- ^ The current state of the ring buffer 
      -> Int       -- ^ The step size
      -> NoBuffer  -- ^ The updated state of the buffer
step' buf step =
  let nextPosition = (position buf + step) `mod` size buf
  in updateTracker buf nextPosition

updateTracker :: NoBuffer -- ^ The current buffer
              -> Int      -- ^ The next position to update
              -> NoBuffer -- ^ The updated buffer
updateTracker (MkNoBuffer _ size' zeroAt' afterZero') nextPosition
  | nextPosition < zeroAt'  = partialBuf (zeroAt' + 1) afterZero'
  | nextPosition == zeroAt' = partialBuf zeroAt' size'
  | otherwise               = partialBuf zeroAt' afterZero'
  where 
    partialBuf = MkNoBuffer (nextPosition + 1) (size' + 1)

