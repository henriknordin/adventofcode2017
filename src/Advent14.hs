module Advent14 
    ( parseInput
    , answer1
    , answer2
    ) where

import qualified Advent10 as A10 (hexadecimal, sparseHash, denseHash, mkA, convert)
import Data.Char (intToDigit)
import Numeric (showIntAtBase, readHex)
import Data.Matrix as M (Matrix, zero, setElem, getElem)
import Data.Graph as G 

parseInput :: String -> String
parseInput = head . lines

answer1 :: String -> Int
answer1 xs = length . filter (== '1') $ concatMap (convertBinary . buildKnotHash) (hashInput xs)

answer2 :: String -> Int
answer2 xs = length . components . buildGraph . buildMatrix $ map (convertBinary . buildKnotHash) (hashInput xs)

test :: String
test = "flqrgnkx" 

hashInput :: String -> [String]
hashInput xs = zipWith (\x suffix -> x ++ "-" ++ show suffix) (replicate 128 xs) [0, 1..]

buildKnotHash :: String -> String
buildKnotHash xs = A10.hexadecimal $ A10.denseHash $ A10.sparseHash (A10.mkA 255) $ A10.convert xs

convertBinary :: String -> String
convertBinary xs = let s = showIntAtBase 2 intToDigit (fst $ head $ readHex xs) ""
             in prependZeros s
  where 
    prependZeros :: String -> String
    prependZeros xs = if length xs < 128 then prependZeros ('0':xs) else xs
    
buildGraph :: M.Matrix Int -> G.Graph 
buildGraph m = let adjList = search m
                   (graph, _, _) = graphFromEdges adjList
               in graph
  where 
    search :: M.Matrix Int -> [(Int, Int, [Int])]
    search m = [(M.getElem r c m, M.getElem r c m, searchEdges r c m) | r <- [1,2..128],
                                                                        c <- [1,2..128],
                                                                        M.getElem r c m > 0] -- && M.getElem (r+1) c m > 0]
    searchEdges :: Int -> Int -> M.Matrix Int -> [Int]
    searchEdges r c m = let n1 = [M.getElem r (c+1) m | c < 128 && M.getElem r (c+1) m > 0]
                            n2 = [M.getElem r (c-1) m | c >   1 && M.getElem r (c-1) m > 0]
                            n3 = [M.getElem (r+1) c m | r < 128 && M.getElem (r+1) c m > 0]
                            n4 = [M.getElem (r-1) c m | r >   1 && M.getElem (r-1) c m > 0]
                         in n1 ++ n2 ++ n3 ++ n4

buildMatrix :: [String] -> M.Matrix Int 
buildMatrix xs = let m = M.zero 128 128
                 in foldl (\m' (x, row) -> updateRow x row m') m (zip xs [1, 2.. 128])
  where
    updateRow :: String -> Int -> M.Matrix Int -> M.Matrix Int
    updateRow xs row m = foldl (\m' (x, col) -> let v = if x == '1' then 128 * (row - 1) + col else 0
                                                in M.setElem v (row, col) m') m (zip xs [1,2.. 128])

