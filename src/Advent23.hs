module Advent23 
    ( parseInput
    , answer1
    , answer2
    ) where

import Data.Char
import Data.Tuple (swap)
import qualified Data.Sequence as S (Seq, replicate, fromList, index, update)

import Debug.Trace

parseInput :: String -> [Op]
parseInput = map parseOp . lines

answer1 :: [Op] -> Int
answer1 = muln . processOps program
  where
    program = Program (S.replicate 8 0) 0 0

answer2 :: [Op] -> S.Seq Int
answer2 = register . processOps program
  where
    register' = S.update 0 1 $ S.replicate 8 0
    program = trace (show register') Program register' 0 0

data Op = Set Int OpInp 
        | Sub Int OpInp
        | Mul Int OpInp
        | Jnz OpInp OpInp
        deriving Show

data OpInp = Letter Int
           | Value  Int
           deriving Show

data Program = Program { register :: S.Seq Int
                       , index :: Int
                       , muln :: Int
                       } deriving Show

processOps :: Program -> [Op] -> Program
processOps program ops = go program (S.fromList ops)
  where
    go :: Program -> S.Seq Op -> Program
    go program ops = let index' = index program
                         op = S.index ops index'
                     in if index' < 0 || index' >= length ops
                          then program
                          --else if S.index (register program) 1 == 1 && index' == 19 
                            --then go (processOp (optimize program)) ops
                            --else 
                          else go (processOp program op) ops 

optimize :: Program -> Program
optimize (Program r i m) = 
  let b = S.index r 1
      d = S.index r 3
      e = S.index r 4
  in if d * e > b 
    then Program r 24 m
    else Program r i m
               

-- Manually transpiled
--
-- a = 1
-- b = 67
-- c = b
-- if a /= 0 then goto A
-- if 1 /= 0 then goto B
-- A: b = b * 100
-- b = b - (-100000)
-- c = b
-- c = c - (-17000)
-- B: H: f = 1
-- d = 2
-- E: e = 2
-- D: g = d
-- g = g * e
-- g = g - b
-- if g /= 0 then goto C
-- f = 0
-- C: e = e - (-1)
-- g = e
-- g = g - b
-- if g /= 0 then goto D
-- d = d - (-1)
-- g = d
-- g = g - b
-- if g /= 0 then goto E
-- if f /= 0 then goto F
-- h = h - (-1)
-- F: g = b
-- g = g -c
-- if g /= 0 then goto G
-- if 1 /= 3 then goto I
-- G: b = b - (-17)
-- if 1 /= 0 then goto H
-- I: END

-- Manually optimized 
--
-- b = 106700
-- c = 123700
-- while (true)
--   f = 1
--   d = 2
--   do {
--     e = 2
--     do {
--       if d * e - b == 0 then 
--         f = 0
--       e = e + 1
--     } while (e - b /= 0) 
--     d = d + 1
--   } while (d - b /= 0)
--   if f == 0 then 
--     h = h + 1
--   if (b - c) == 0 then 
--     b = b + 17
--   else 
--     return h
-- }


--optimize :: Program -> Int -> Program
--optimize p 19 = let r = register p
--                    valE = S.index r 4
--                    valG = S.index r 6
--                    r' = S.update 4 (valE - valG) $ S.update 6 0 r
--                    r'' = if S.index r 3 * (S.index r 4 - 1)  - S.index r 1 == 0 
--                            then S.update 5 0 r'
--                            else r'
--                in Program r'' (index p + 1) (muln p)
--optimize p 23 = let r = register p
--                    valD = S.index r 3
--                    valG = S.index r 6
--                    r' = S.update 3 (valD - valG) (S.update 6 0 r)
--                    r'' = if (S.index r 3 - 1) * (S.index r 4 - 1) - S.index r 1 == 0 
--                            then S.update 5 0 r'
--                            else r'
--                in Program r'' (index p + 1) (muln p)

processOp :: Program -> Op -> Program
processOp (Program r i n) (Set a (Letter b))  = Program (S.update a (S.index r b) r) (i + 1) n
processOp (Program r i n) (Set a (Value b))   = Program (S.update a b r) (i + 1) n
processOp (Program r i n) (Sub a (Letter b))  = Program (S.update a (S.index r a - S.index r b) r) (i + 1) n
processOp (Program r i n) (Sub a (Value b))   = Program (S.update a (S.index r a - b) r) (i + 1) n
processOp (Program r i n) (Mul a (Letter b))  = Program (S.update a (S.index r a * S.index r b) r) (i + 1) (n + 1)
processOp (Program r i n) (Mul a (Value b))   = Program (S.update a (S.index r a * b) r) (i + 1) (n + 1)
processOp (Program r i n) (Jnz (Letter a) (Letter b))  = if S.index r a /= 0 then Program r (i + S.index r b) n else Program r (i + 1) n
processOp (Program r i n) (Jnz (Letter a) (Value b))   = if S.index r a /= 0 then Program r (i + b) n else Program r (i + 1) n
processOp (Program r i n) (Jnz (Value a) (Letter b))  = if a /= 0 then Program r (i + S.index r b) n else Program r (i + 1) n
processOp (Program r i n) (Jnz (Value a) (Value b))   = if a /= 0 then Program r (i + b) n else Program r (i + 1) n

parseOp :: String -> Op
parseOp = go . words
  where
    go :: [String] -> Op
    go ["set", [reg], v] = Set (regIndex reg) (parseOpInp v)
    go ["sub", [reg], v] = Sub (regIndex reg) (parseOpInp v)
    go ["mul", [reg], v] = Mul (regIndex reg) (parseOpInp v)
    go ["jnz", reg,   v] = Jnz (parseOpInp reg) (parseOpInp v)

parseOpInp :: String -> OpInp
parseOpInp s
  | length s == 1 && ord (head s) >= ord 'a' = Letter (regIndex $ head s)
  | otherwise                                = Value (read s :: Int)

regIndex :: Char -> Int
regIndex c = ord c - ord 'a'

