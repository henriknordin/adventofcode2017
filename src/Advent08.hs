module Advent08 
    ( parseInput
    , input
    , parse
    , process
    , advent8
    ) where

import Data.List
import qualified Data.Map as M

data Instruction = 
  MkInstruction { register  :: String
                , op        :: String
                , value     :: Int
                , condition :: Condition 
                } deriving Show

data Condition = 
  MkCondition { cRegister :: String
              , cOp       :: String
              , cValue    :: Int
              } deriving Show

parseInput :: String -> [Instruction]
parseInput = map parse . lines

parse :: String -> Instruction
parse x = let sp = words x
              cond = MkCondition (sp !! 4) (sp !! 5) (read (sp !! 6) :: Int)
          in MkInstruction (head sp)
                           (sp !! 1)
                           (read (sp !! 2) :: Int)
                           cond
                                                                           
input :: [String]
input = 
  [ "b inc 5 if a > 1"
  , "a inc 1 if b < 5"
  , "c dec -10 if a >= 1"
  , "c inc -20 if c == 10"]

process :: [Instruction] -> (M.Map String Int, Int)
process = foldl single (M.empty, 0)
  where
    single :: (M.Map String Int, Int) -> Instruction -> (M.Map String Int, Int)
    single (mp, prev) i = let register' = register i
                              val = M.findWithDefault 0 register' mp
                              con = condition i
                              cVal = M.findWithDefault 0 (cRegister con) mp
                              newVal = evalOp val (op i) (value i)
                              maxi = maximum [newVal, prev]
                          in if predicate con mp then (M.insert register' newVal mp, maxi) else (mp, prev)

predicate :: Condition -> M.Map String Int -> Bool
predicate c mp = let actual = M.findWithDefault 0 (cRegister c) mp
                     cO = cOp c
                     cVal =   cValue c
                 in evaluate actual cO cVal

evalOp :: Int -> String -> Int -> Int
evalOp a "inc" b = a + b
evalOp a "dec" b = a - b

evaluate :: Int -> String -> Int -> Bool
evaluate a ">=" b = a >= b
evaluate a ">" b = a > b
evaluate a "<=" b = a <= b
evaluate a "<" b = a < b
evaluate a "==" b = a == b
evaluate a "!=" b = a /= b

advent8 :: M.Map String Int -> Int
advent8 mp = maximum $ M.elems mp

