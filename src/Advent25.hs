module Advent25
  ( parseInput
  , answer
  ) where

import           Text.Megaparsec        (string)
import           Text.Megaparsec.Char   (letterChar)
import qualified Text.Megaparsec.Lexer  as L (integer)
import           Text.Megaparsec.String

import qualified Data.Map.Strict        as Map (Map, delete, empty,
                                                findWithDefault, fromList,
                                                insert, lookup, size)
import           Data.Maybe             (fromJust)

import           Control.Applicative

parseInput :: Parser (String, Int, [(String, State)])
parseInput = do
  start <- string "Begin in state " *> some letterChar <* string ".\n"
  iter <- string "Perform a diagnostic checksum after " *> L.integer <* string " steps.\n"
  rules <- many parseState
  pure (start, fromIntegral iter, rules)

parseState :: Parser (String, State)
parseState = do
  name <- string "\nIn state " *> some letterChar <* string ":\n"
  a0 <- parseAction $ string "0"
  a1 <- parseAction $ string "1"
  pure (name, State a0 a1)

parseAction :: Parser a -> Parser Action
parseAction p = do
  string "  If the current value is " *> p *> string ":\n"
  w <- string "    - Write the value " *> parseValue <* string ".\n"
  d <- string "    - Move one slot to the " *> parseDirection <* string ".\n"
  n <- string "    - Continue with state " *> some letterChar <* string ".\n"
  pure (Action w d n)

parseValue :: Parser Bool
parseValue = True <$ string "1" <|> False <$ string "0"

parseDirection :: Parser Direction
parseDirection = R <$ string "right" <|> L <$ string "left"

answer :: String -> Int -> [(String, State)] -> Int
answer start iter xs = checksum $ process startState 0 iter Map.empty getState
  where
    getState :: String -> State
    getState k =
      let m = Map.fromList xs
      in fromJust $ Map.lookup k m
    startState:: State
    startState = getState start

process :: State -> Int -> Int -> Map.Map Int Bool -> (String -> State) -> Map.Map Int Bool
process s i 0 tape ss = tape
process (State f t) i n tape getState =
  let value = Map.findWithDefault False i tape
      action =
        if value
          then t
          else f
      i' =
        if move action == L
          then i - 1
          else i + 1
      tape' =
        if write action
          then Map.insert i True tape
          else Map.delete i tape
      s' = getState $ next action
  in process s' i' (n - 1) tape' getState

data Direction
  = L
  | R
  deriving (Show, Eq)

data Action = Action
  { write :: Bool
  , move  :: Direction
  , next  :: String
  } deriving (Show)

data State =
  State !Action
        !Action
  deriving (Show)

checksum :: Map.Map Int Bool -> Int
checksum = Map.size

