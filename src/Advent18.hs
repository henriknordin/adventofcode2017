module Advent18 
    ( answer1
    , answer2
    ) where

import Data.Char
import Data.Tuple (swap)
import qualified Data.Sequence as S (Seq, replicate, fromList, index, update)

answer1 :: [String] -> Int
answer1 = head . send . processOps . parse

answer2 :: [String] -> Int
answer2 xs = let (p, p') = processPrograms $ parse xs
             in sendn $ head $ filter (\p -> pId p == 1) [p, p']

test1 :: [String]
test1 = 
    [ "set a 1"
    , "add a 2"
    , "mul a a"
    , "mod a 5"
    , "snd a"
    , "set a 0"
    , "rcv a"
    , "jgz a -1"
    , "set a 1"
    , "jgz a -2"]

test2 :: [String]
test2 = 
    [ "snd 1"
    , "snd 2"
    , "snd p"
    , "rcv a"
    , "rcv b"
    , "rcv c"
    , "rcv d"]

data Op = Snd OpInp
        | Set Int OpInp 
        | Add Int OpInp
        | Mul Int OpInp
        | Mod Int OpInp
        | Rcv Int
        | Jgz OpInp OpInp
        deriving Show

data OpInp = Letter Int
           | Value  Int
           deriving Show

data Program = Program { pId :: Int
                       , register :: S.Seq Int
                       , index :: Int
                       , send  :: [Int]
                       , recv  :: [Int]
                       , sendn :: Int
                       } deriving Show

processOps :: [Op] -> Program
processOps ops = let program = Program 0 (S.replicate 16 0) 0 [] [] 0
                 in go program (S.fromList ops)
  where
    go :: Program -> S.Seq Op -> Program
    go program ops = let index' = index program
                         op = S.index ops index'
                     in case op of
                       (Rcv a)   -> if S.index (register program) a <= 0 then go (processOp program op) ops else program
                       _         -> go (processOp program op) ops 

deadlock :: S.Seq Op -> Program -> Program -> Bool
deadlock ops (Program _ r i s rec _) (Program _ r' i' s' rec' _) = let op = S.index ops i
                                                                       op' = S.index ops i'
                                                                   in isRcv op && isRcv op' && null rec && null rec'
  where
    isRcv :: Op -> Bool
    isRcv (Rcv _ ) = True
    isRcv _        = False

processPrograms :: [Op] -> (Program, Program)
processPrograms ops = let program0 = Program 0 (S.update (regIndex 'p') 0 $ S.replicate 16 0) 0 [] [] 0
                          program1 = Program 1 (S.update (regIndex 'p') 1 $ S.replicate 16 0) 0 [] [] 0
                      in go (program0, program1) (S.fromList ops)
  where
    go :: (Program, Program) -> S.Seq Op -> (Program, Program)
    go programs ops = let ind = index $ fst programs
                          ind' = index $ snd programs
                          op = S.index ops ind
                          op' = S.index ops ind'
                          recvQ = recv $ fst programs
                          deadlock' = uncurry (deadlock ops) programs
                      in if deadlock' || ((ind < 0 || ind >= length ops) && (ind' < 0 || ind' >= length ops))
                           then programs 
                           else go (sendQueue (processOp (fst programs) op, processOp (snd programs) op')) ops 
                           

processOp :: Program -> Op -> Program
processOp (Program p r i s rec n) (Snd (Letter a))    = Program p r (i + 1) (S.index r a : s) rec (n+1)
processOp (Program p r i s rec n) (Snd (Value a))     = Program p r (i + 1) (a : s) rec (n+1)
processOp (Program p r i s rec n) (Set a (Letter b))  = Program p (S.update a (S.index r b) r) (i + 1) s rec n
processOp (Program p r i s rec n) (Set a (Value b))   = Program p (S.update a b r) (i + 1) s rec n
processOp (Program p r i s rec n) (Add a (Letter b))  = Program p (S.update a (S.index r a + S.index r b) r) (i + 1) s rec n
processOp (Program p r i s rec n) (Add a (Value b))   = Program p (S.update a (S.index r a + b) r) (i + 1) s rec n
processOp (Program p r i s rec n) (Mul a (Letter b))  = Program p (S.update a (S.index r a * S.index r b) r) (i + 1) s rec n
processOp (Program p r i s rec n) (Mul a (Value b))   = Program p (S.update a (S.index r a * b) r) (i + 1) s rec n
processOp (Program p r i s rec n) (Mod a (Letter b))  = Program p (S.update a (S.index r a `mod` S.index r b) r) (i + 1) s rec n
processOp (Program p r i s rec n) (Mod a (Value b))   = Program p (S.update a (S.index r a `mod` b) r) (i + 1) s rec n 
processOp (Program p r i s rec n) (Jgz (Letter a) (Letter b))  = if S.index r a > 0 then Program p r (i + S.index r b) s rec n else Program p r (i + 1) s rec n
processOp (Program p r i s rec n) (Jgz (Letter a) (Value b))   = if S.index r a > 0 then Program p r (i + b) s rec n else Program p r (i + 1) s rec n
processOp (Program p r i s rec n) (Jgz (Value a) (Letter b))  = if a > 0 then Program p r (i + S.index r b) s rec n else Program p r (i + 1) s rec n
processOp (Program p r i s rec n) (Jgz (Value a) (Value b))   = if a > 0 then Program p r (i + b) s rec n else Program p r (i + 1) s rec n
processOp (Program p r i s []  n) (Rcv a)             = Program p r i s [] n 
processOp (Program p r i s (rec':rec) n) (Rcv a)      = Program p (S.update a rec' r) (i + 1) s rec n

sendQueue :: (Program, Program) -> (Program, Program)
sendQueue (Program p r i s rec n, Program p' r' i' s' rec' n') = (Program p r i [] (rec ++ s') n, Program p' r' i' [] (rec' ++ s) n')

parse :: [String] -> [Op]
parse = map parseOp

parseOp :: String -> Op
parseOp = go . words
  where
    go :: [String] -> Op
    go ["set", [reg], v] = Set (regIndex reg) (parseOpInp v)
    go ["add", [reg], v] = Add (regIndex reg) (parseOpInp v)
    go ["mul", [reg], v] = Mul (regIndex reg) (parseOpInp v)
    go ["mod", [reg], v] = Mod (regIndex reg) (parseOpInp v)
    go ["jgz", reg,   v] = Jgz (parseOpInp reg) (parseOpInp v)
    go ["snd", reg]      = Snd (parseOpInp reg) 
    go ["rcv", [reg]]    = Rcv (regIndex reg) 

parseOpInp :: String -> OpInp
parseOpInp s
  | length s == 1 && ord (head s) >= ord 'a' = Letter (regIndex $ head s)
  | otherwise                                = Value (read s :: Int)

regIndex :: Char -> Int
regIndex c = ord c - ord 'a'

