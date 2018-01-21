module Lib
    ( getInput
    , getParsed
    ) where

import System.IO (readFile)
import Text.Printf (printf)

import Text.Megaparsec (parse, parseErrorPretty)
import Text.Megaparsec.String

getInput :: Int -> IO String
getInput i = readFile (printf "data/input%02d.txt" i)

getParsed :: Parser a -> String -> IO a
getParsed p s = case parse p "dummy.txt" s of
            Left err -> fail (parseErrorPretty err)
            Right a  -> return a
