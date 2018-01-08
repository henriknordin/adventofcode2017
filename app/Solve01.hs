module Main (main) where

import System.IO (readFile)
import Lib (getInput)

import Advent01 (parseInput, captcha1, captcha2)

main :: IO ()
main = do 
  input <- parseInput <$> getInput 1
  putStrLn $ "Advent 1-1: " ++ show (captcha1 input)  -- 1223
  putStrLn $ "Advent 1-2: " ++ show (captcha2 input)  -- 1284
