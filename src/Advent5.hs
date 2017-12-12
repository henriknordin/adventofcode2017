module Advent5
    ( answer1
    , answer2

    , input5
    , solve''
    , solve2''

    ) where

import qualified Data.Sequence as S (Seq, index, length, update, elemIndexL, fromList)

answer1 :: [String] -> Int
answer1 xs = solve'' 0 0 (S.fromList $ concat $ parse xs)

answer2 :: [String] -> Int
answer2 xs = solve2'' 0 0 (S.fromList $ concat $ parse xs)

-- Move to separate module?
parse :: [String] -> [[Int]]
parse xs = map (map (\x -> read x :: Int)) $ map words xs

--input5 :: String
input5 = map (\x -> read x :: Int) $ words "0 3 0 1 -3"

solve'' :: Int -> Int -> S.Seq Int -> Int
solve'' n ind xs = let value = S.index xs ind
                   in if ind >= S.length xs then n else solve'' (n + 1) (ind + value) (S.update ind (value + 1) xs)

solve2'' :: Int -> Int -> S.Seq Int -> Int
solve2'' n ind xs = let value = S.index xs ind
                    in if ind >= S.length xs then n else solve2'' (n + 1) (ind + value) (S.update ind (if value < 3 then value + 1 else value - 1) xs) 
