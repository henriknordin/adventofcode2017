module Lib
    ( captcha
    , captcha2
    , input
    , checksum
    , checksum2
    , naturals
    , edges
    , manhattan
    , graph
    , valid
    , valid2
    , input5
    , parse
    , solve''
    , solve2''
    , input6
    , dists

    ) where

import Data.Char (digitToInt)
import Data.List (sort, group, unfoldr, isInfixOf, partition, find, sortBy)
import qualified Data.Sequence as S (Seq, index, length, update, elemIndexL, fromList)
import Data.Maybe (fromJust, isJust)


captcha :: String -> Int
captcha xs = calculate (digitToInt $ head xs) (map digitToInt xs)
  where
    calculate :: Int -> [Int] -> Int
    calculate a [x] = if a == x then x else 0
    calculate a (x:y:xs) = s + calculate a (y:xs) 
                           where s = if x == y then x else 0


captcha2 :: String -> Int
captcha2 xs = calculate $ splitHalf $ map digitToInt xs
  where
    calculate :: ([Int], [Int]) -> Int
    calculate (xs, ys) = sum $ map (\(a, _) -> 2 * a) $ filter (\(a, b) -> a == b) $ zip xs ys
                     
splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt half xs
               where half = length xs `div` 2 

input :: [String]
input =
  [ "1136    1129    184     452     788     1215    355     1109    224     1358    1278    176     1302    186     128     1148"
  , " 242     53      252     62      40      55      265     283     38      157     259     226     322     48      324     299"
  , "2330    448     268     2703    1695    2010    3930    3923    179     3607    217     3632    1252    231     286     3689"
  , "  89      92      903     156     924     364     80      992     599     998     751     827     110     969     979     734"
  , " 100     304     797     81      249     1050    90      127     675     1038    154     715     79      1116    723     990"
  , "1377    353     3635    99      118     1030    3186    3385    1921    2821    492     3082    2295    139     125     2819"
  , "3102    213     2462    116     701     2985    265     165     248     680     3147    1362    1026    1447    106     2769"
  , "5294    295     6266    3966    2549    701     2581    6418    5617    292     5835    209     2109    3211    241     5753"
  , " 158     955     995     51      89      875     38      793     969     63      440     202     245     58      965     74"
  , "  62      47      1268    553     45      60      650     1247    1140    776     1286    200     604     399     42      572"
  , " 267     395     171     261     79      66      428     371     257     284     65      25      374     70      389     51"
  , "3162    3236    1598    4680    2258    563     1389    3313    501     230     195     4107    224     225     4242    4581"
  , " 807     918     51      1055    732     518     826     806     58      394     632     36      53      119     667     60"
  , " 839     253     1680    108     349     1603    1724    172     140     167     181     38      1758    1577    748     1011"
  , "1165    1251    702     282     1178    834     211     1298    382     1339    67      914     1273    76      81      71"
  , "6151    5857    4865    437     6210    237     37      410     544     214     233     6532    2114    207     5643    6852"]

checksum :: [String] -> Int
checksum xs = sum $ map (\xs -> maximum xs - minimum xs) $ parse xs

checksum2 :: [String] -> Int
checksum2 xs = sum $ map evenDivisable $ parse xs
  where
    evenDivisable :: [Int] -> Int
    evenDivisable xs' = head [ x `div` y | x <- xs', y <- xs', x `mod` y == 0 && x /= y]

-- 1 2 3
-- 2 
-- 3

parse :: [String] -> [[Int]]
parse xs = map (map (\x -> read x :: Int)) $ map words xs

data Spiral = Start
              | R   
              | U  
              | L 
              | D 
              deriving (Show)

naturals :: [Int]
naturals = iterate (+1) 1

edges :: [Int]
edges = concatMap (replicate 2) [1, 2..]

graph :: [Spiral]
graph = concat $ zipWith (\a b -> replicate a b) edges (cycle [R, U, L, D])   

manhattan n = (\(h, v) -> abs h + abs v) $ foldr step (0, 0) $ take (n - 1) graph
  where
    step R (h, v) = (h + 1, v)
    step U (h, v) = (h, v + 1)
    step L (h, v) = (h - 1, v)
    step D (h, v) = (h, v - 1)


valid :: String -> Bool
valid xs = all (\x -> length x == 1) $ group $ sort $ words xs

valid2 :: String -> Bool
valid2 xs = all (\x -> length x == 1) $ group $ sort $ map sort $ words xs


-- parse xs = map (map (\x -> read x :: Int)) $ map words xs
--input5 :: String
input5 = map (\x -> read x :: Int) $ words "0 3 0 1 -3"

solve'' :: Int -> Int -> S.Seq Int -> Int
solve'' n ind xs = let value = S.index xs ind
                   in if ind >= S.length xs then n else solve'' (n + 1) (ind + value) (S.update ind (value + 1) xs)

solve2'' :: Int -> Int -> S.Seq Int -> Int
solve2'' n ind xs = let value = S.index xs ind
                    in if ind >= S.length xs then n else solve2'' (n + 1) (ind + value) (S.update ind (if value < 3 then value + 1 else value - 1) xs) 
input6 :: S.Seq Int
input6 = S.fromList [14, 0, 15, 12, 11, 11, 3, 5, 1, 6, 8, 4, 9, 1, 8, 4]

dists :: S.Seq Int -> (S.Seq Int, Int)
dists xs = dists 1 [] xs
  where 
    dists cycles combs xs' = let next = redistribute xs'
                                 nextCombs = next : combs
                      in if next `elem` combs then (next, cycles) else dists (cycles +1) nextCombs next

-- advent 6-2: use input of advent 6-1 - 1 => 1037

redistribute :: S.Seq Int ->  S.Seq Int
redistribute xs = let blocks = maximum xs
                      ind    = fromJust $ S.elemIndexL blocks xs 
                  in redistribute' blocks ind (S.update ind 0 xs)
  where 
    redistribute' :: Int -> Int -> S.Seq Int -> S.Seq Int
    redistribute' blocks' ind' xs' = let nextInd = if ind' == length xs' - 1 then 0 else ind' + 1
                                         value = S.index xs' nextInd
                                       in if blocks' > 0 then redistribute' (blocks' - 1) nextInd (S.update nextInd (value + 1) xs') else xs'




