module Advent7
    ( input7
    , buildTree 
    , root
    , unbalanced
    ) where


import Data.List (sort, group, find)
import qualified Data.Tree as T 
import Data.Maybe (fromJust)
import qualified Data.Map as M 

input7 :: [String]
input7 =  
  [ "pbga (66)"
  , "xhth (57)"
  , "ebii (61)"
  , "havc (66)"
  , "ktlj (57)"
  , "fwft (72) -> ktlj cntj xhth"
  , "qoyq (66)"
  , "padx (45) -> pbga havc qoyq"
  , "tknk (41) -> ugml padx fwft"
  , "jptl (61)"
  , "ugml (68) -> gyxo ebii jptl"
  , "gyxo (61)"
  , "cntj (57)"]

data Program = Program { name :: String 
                       , weight :: Int  
                       } deriving Show

buildTree :: [String] -> T.Tree Program
buildTree xs = let parsed = map parse xs
                   children = foldr (\(_, children) acc -> children ++ acc) [] parsed 
                   kv = foldr (\(program, children) mp -> M.insert (name program) (program, children) mp) M.empty parsed
                   root = head $ filter (\(p, _) -> not $ isChild (name p) children) parsed
               in build root kv
  where 
    parse :: String -> (Program, [String])
    parse s = let comps = words s
                  name = head comps
                  weight = read (takeWhile (/= ')') $ tail $ dropWhile (/= '(') (comps !! 1)) :: Int
                  children = dropWhile (/= "->") comps
                  p = Program name weight
              in (p, if null children then [] else tail children)
    isChild :: String -> [String] -> Bool
    isChild x xs = x `elem` xs
    build :: (Program, [String]) -> M.Map String (Program, [String]) -> T.Tree Program
    build (p, ch) kv = T.Node p (map (\n -> build (fromJust $ M.lookup n kv) kv) ch)

root :: T.Tree Program -> String
root (T.Node (Program name _) _) = name

unbalanced :: T.Tree Program -> Int-> Int
unbalanced (T.Node (Program _ w) []) _ = w
unbalanced (T.Node (Program n w) fs) prev = let ws = map weight fs
                                                ub = maximum ws - minimum ws -- any of the children unbalanced -> if not, return the disc value
                                                xxx = head $ head $ filter (\x -> length x == 1) $ group $ sort ws
                                                p = fromJust $ find (\q -> xxx == weight q) fs
                                       in if ub /= 0 then unbalanced p ub else w - prev 

  where 
    weight :: T.Tree Program -> Int
    weight (T.Node (Program _ w) []) = w
    weight (T.Node (Program _ w) fs) = w + sum (map weight fs)

