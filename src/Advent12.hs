module Advent12 
    ( parseInput
    , answer1
    , countGroups
    ) where

import Data.List.Split (splitOn)
import Data.Graph as G

test :: [String]
test = 
  [ "0 <-> 2"
  , "1 <-> 1"
  , "2 <-> 0, 3, 4"
  , "3 <-> 2, 4"
  , "4 <-> 2, 3, 6"
  , "5 <-> 6"
  , "6 <-> 4, 5"]

parseInput :: String -> Graph
parseInput = buildGraph . adjacencyList. map parse . lines

parse :: String -> [String]
parse = splitOn "<->" . removeWhitespace

removeWhitespace :: String -> String
removeWhitespace = filter (/= ' ')

buildGraph :: [(Int, Int, [Int])] -> Graph
buildGraph xs = let (graph, _, _) = graphFromEdges xs
                in graph

adjacencyList :: [[String]] -> [(Int, Int, [Int])]
adjacencyList  = map adjacency

adjacency :: [String] -> (Int, Int, [Int])
adjacency xs = let vertex = read (head xs) :: Int
                   neighbors = map (\x -> read x :: Int) $ splitOn "," (head $ tail xs)
               in (vertex, vertex, neighbors)

countVertices :: Graph -> Vertex -> Int
countVertices g v = length $ reachable g v

countGroups :: Graph -> Int
countGroups = length . components 

answer1 :: Graph -> Int
answer1 g = countVertices g 0

