module Main where

import qualified Data.Map.Strict as Map (Map, empty, findWithDefault, lookup, insert, delete, size)
import Data.Maybe (fromJust)

answer1 :: Int
answer1 = checksum $ process (head states) 0 12523873 Map.empty (toMap states)
  where
    states = 
      [ State 'A' (Action True R 'B') (Action True L 'E')
      , State 'B' (Action True R 'C') (Action True R 'F')
      , State 'C' (Action True L 'D') (Action False R 'B')
      , State 'D' (Action True R 'E') (Action False L 'C')
      , State 'E' (Action True L 'A') (Action False R 'D')
      , State 'F' (Action True R 'A') (Action True R 'C')
      ]

--Begin in state A.
--Perform a diagnostic checksum after 6 steps.
--
--In state A:
--  If the current value is 0:
--    - Write the value 1.
--    - Move one slot to the right.
--    - Continue with state B.
--  If the current value is 1:
--    - Write the value 0.
--    - Move one slot to the left.
--    - Continue with state B.
--
--In state B:
--  If the current value is 0:
--    - Write the value 1.
--    - Move one slot to the left.
--    - Continue with state A.
--  If the current value is 1:
--    - Write the value 1.
--    - Move one slot to the right.
--    - Continue with state A.

test :: [State] 
test = 
  [ State 'A' (Action True R 'B') (Action False L 'B')
  , State 'B' (Action True L 'A') (Action True  R 'A')
  ]

toMap :: [State] -> Map.Map Char State
toMap = foldr (\s b -> Map.insert (name s) s b) Map.empty

data Direction = L | R deriving (Show, Eq)

data Action = Action { write :: Bool
                     , move  :: Direction
                     , next  :: Char
                     } deriving (Show)
               
data State = State { name  :: Char
                   , false :: Action
                   , true  :: Action
                   } deriving (Show)

process :: State -> Int -> Int -> Map.Map Int Bool -> Map.Map Char State -> Map.Map Int Bool
process s i 0 tape ss = tape
process (State _ f t) i n tape ss = 
  let value = Map.findWithDefault False i tape
      action = if value then t else f
      i' = if move action == L then i - 1 else i + 1
      tape' = if write action then Map.insert i True tape else Map.delete i tape
      s' = fromJust $ Map.lookup (next action) ss 
  in process s' i' (n-1) tape' ss

checksum :: Map.Map Int Bool -> Int
checksum = Map.size

main :: IO ()
main = 
  putStrLn $ "Answer 1: " ++ show answer1
