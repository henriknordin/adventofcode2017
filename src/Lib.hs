module Lib
    ( getInput
    ) where

import System.IO (readFile)
import Text.Printf (printf)

getInput :: Int -> IO String
getInput i = readFile (printf "data/input%02d.txt" i)

