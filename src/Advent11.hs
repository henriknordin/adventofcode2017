module Advent11 where

import Data.List.Split (splitOn)

test1 = "ne,ne,ne"
test2 = "ne,ne,sw,sw"
test3 = "ne,ne,s,s"
test4 = "se,sw,se,sw,sw"

input = "se,ne,ne,n,nw,s,sw,sw,n,nw,se,nw,s,nw,nw,nw,sw,sw,sw,sw,se,n,sw,sw,s,sw,sw,sw,s,s,s,s,sw,s,n,s,sw,s,s,s,s,s,s,se,s,s,se,s,se,se,nw,ne,se,se,nw,se,se,se,n,se,se,se,nw,se,se,se,se,se,se,ne,se,ne,se,ne,ne,se,ne,ne,se,s,s,sw,ne,ne,n,ne,nw,ne,ne,sw,ne,se,ne,ne,ne,ne,ne,ne,nw,ne,n,ne,nw,n,ne,se,ne,s,n,se,sw,ne,n,ne,ne,n,n,sw,n,ne,nw,ne,n,ne,n,n,n,n,nw,n,n,n,n,ne,sw,n,n,n,n,ne,se,n,n,n,n,n,n,nw,sw,n,n,n,n,n,n,n,n,n,n,n,n,nw,n,nw,n,n,sw,nw,sw,n,ne,n,n,s,n,n,ne,n,nw,n,ne,nw,nw,n,n,n,ne,se,nw,n,ne,nw,nw,n,n,nw,nw,nw,nw,s,nw,nw,nw,nw,nw,sw,n,nw,nw,nw,nw,n,nw,nw,nw,nw,nw,sw,nw,nw,nw,nw,nw,nw,ne,ne,n,se,nw,nw,nw,se,nw,nw,nw,nw,nw,nw,nw,ne,nw,nw,sw,nw,sw,n,nw,se,sw,nw,se,sw,nw,nw,nw,ne,sw,sw,n,sw,nw,sw,nw,sw,nw,sw,nw,sw,sw,nw,s,nw,nw,nw,sw,nw,ne,sw,sw,nw,sw,nw,sw,nw,se,nw,sw,nw,n,nw,nw,nw,sw,nw,sw,sw,sw,nw,sw,nw,nw,sw,sw,sw,nw,sw,sw,s,nw,sw,ne,sw,nw,sw,nw,sw,sw,sw,sw,sw,sw,sw,sw,nw,sw,sw,n,sw,sw,sw,ne,sw,s,se,sw,sw,nw,sw,sw,sw,sw,sw,nw,sw,sw,sw,sw,s,sw,sw,ne,sw,sw,sw,ne,n,sw,sw,sw,ne,s,ne,sw,ne,sw,s,sw,sw,sw,se,nw,sw,sw,sw,sw,s,n,sw,sw,sw,sw,nw,sw,sw,se,s,sw,sw,n,sw,sw,s,sw,ne,se,sw,sw,sw,sw,sw,s,s,s,se,sw,sw,s,s,sw,sw,s,sw,nw,s,s,sw,s,ne,sw,ne,s,s,s,sw,s,sw,s,sw,s,ne,nw,sw,nw,sw,sw,s,sw,sw,s,s,s,sw,s,s,s,s,s,s,s,s,s,se,nw,se,n,s,s,s,sw,sw,s,sw,ne,s,ne,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,n,nw,s,s,s,s,sw,s,s,s,s,s,s,s,s,s,se,s,s,s,sw,ne,s,se,s,s,s,s,se,s,s,s,se,s,s,sw,s,s,s,se,se,se,se,sw,se,s,s,ne,s,n,ne,s,s,sw,sw,s,s,s,se,n,nw,s,n,s,se,se,nw,s,se,nw,ne,s,s,se,sw,sw,ne,n,se,se,s,nw,n,s,s,se,s,se,se,se,s,se,sw,sw,se,s,s,se,s,se,n,s,s,se,se,sw,se,se,ne,s,s,se,se,nw,s,s,s,s,sw,se,ne,se,s,se,se,se,n,s,nw,se,se,se,se,ne,s,se,n,se,se,s,s,se,se,sw,se,s,se,se,s,se,se,se,se,se,se,s,nw,se,se,se,se,se,se,se,se,se,s,sw,n,se,se,s,se,ne,se,s,n,se,se,se,se,se,se,se,nw,se,se,se,sw,s,se,se,se,se,se,se,ne,s,se,n,se,se,se,se,s,ne,se,s,s,se,se,n,s,se,se,se,se,n,se,se,se,se,se,n,sw,se,se,se,se,se,se,se,se,se,se,se,se,se,se,ne,se,n,se,se,se,nw,ne,se,n,s,s,ne,se,se,se,se,se,ne,se,se,ne,se,se,sw,se,se,se,n,se,se,se,n,se,se,se,se,se,nw,se,nw,se,se,sw,se,sw,se,se,n,ne,se,n,se,s,ne,se,se,se,nw,ne,se,s,nw,ne,se,ne,se,se,se,ne,se,ne,nw,se,sw,se,se,n,se,ne,n,se,sw,se,se,se,se,sw,ne,se,se,sw,se,se,se,nw,ne,se,n,se,se,ne,se,ne,se,nw,ne,ne,se,sw,ne,se,se,sw,se,se,se,ne,se,ne,ne,se,ne,sw,se,se,ne,ne,se,se,se,ne,se,s,ne,se,se,n,ne,ne,se,se,ne,ne,ne,ne,ne,se,ne,ne,ne,se,s,ne,s,ne,nw,n,ne,se,se,se,n,se,ne,ne,se,ne,ne,ne,ne,ne,ne,ne,s,ne,ne,ne,ne,s,sw,nw,se,ne,ne,ne,ne,ne,ne,sw,se,ne,sw,se,s,ne,ne,ne,se,ne,n,ne,se,sw,se,n,ne,ne,nw,ne,ne,ne,n,s,ne,ne,ne,ne,ne,ne,nw,ne,sw,ne,se,ne,se,ne,ne,sw,n,se,ne,s,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,se,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,n,ne,sw,ne,ne,ne,ne,ne,ne,ne,ne,s,n,ne,ne,ne,ne,ne,ne,se,ne,ne,ne,nw,ne,ne,n,ne,ne,ne,ne,ne,se,ne,se,s,sw,ne,ne,ne,ne,ne,n,nw,ne,s,ne,ne,ne,ne,n,ne,ne,ne,ne,n,ne,sw,ne,ne,ne,s,ne,ne,ne,ne,n,nw,ne,n,ne,n,s,ne,ne,ne,ne,se,ne,ne,ne,ne,sw,n,ne,ne,s,nw,ne,n,ne,ne,ne,sw,nw,n,n,s,n,n,ne,n,ne,ne,ne,n,se,ne,sw,s,se,ne,ne,ne,s,nw,s,n,s,ne,n,ne,sw,ne,ne,sw,ne,sw,s,ne,ne,ne,se,n,n,se,ne,n,ne,ne,ne,ne,ne,ne,ne,n,se,n,n,n,s,ne,ne,ne,ne,ne,n,n,ne,sw,nw,se,ne,n,nw,ne,ne,ne,n,ne,ne,n,ne,nw,ne,nw,se,nw,ne,n,ne,ne,n,ne,n,ne,ne,nw,sw,ne,n,n,nw,se,sw,s,n,n,n,n,ne,n,n,se,nw,n,n,nw,ne,n,nw,n,ne,ne,ne,ne,n,ne,n,ne,n,n,se,n,sw,n,n,se,sw,ne,n,n,n,nw,n,n,ne,ne,n,ne,ne,ne,ne,n,se,ne,n,ne,se,n,ne,n,ne,s,ne,ne,n,n,n,ne,sw,n,n,n,ne,se,n,ne,n,ne,ne,n,n,n,ne,n,s,n,n,sw,n,n,n,ne,n,s,n,n,ne,s,n,s,s,se,ne,s,ne,se,n,n,n,n,n,n,n,n,n,n,ne,n,n,n,n,s,nw,n,sw,ne,n,ne,n,s,n,n,s,n,n,n,n,s,n,n,n,sw,n,se,nw,s,ne,n,nw,se,n,n,s,ne,s,n,n,n,sw,n,n,n,nw,sw,n,sw,n,ne,n,n,n,s,s,n,n,n,ne,n,n,n,n,ne,se,n,n,n,n,n,n,n,n,ne,n,n,se,s,n,n,n,n,n,n,se,s,se,n,n,n,n,n,n,n,sw,n,ne,n,se,n,n,n,n,nw,n,n,n,n,s,n,n,n,n,n,n,n,n,n,se,n,s,n,n,se,n,n,n,s,n,nw,n,n,nw,nw,n,n,se,n,n,se,n,ne,nw,n,n,se,n,nw,sw,se,n,sw,n,se,nw,n,n,n,n,nw,n,n,n,n,sw,n,nw,n,sw,n,ne,n,nw,s,n,n,nw,n,nw,n,n,sw,nw,n,n,n,n,n,n,n,n,n,n,n,n,n,n,se,ne,n,se,nw,sw,sw,n,n,n,n,nw,s,s,n,n,nw,n,n,n,n,ne,se,n,n,n,n,n,n,n,n,n,n,n,nw,n,nw,n,n,sw,sw,n,n,se,n,n,nw,n,se,sw,n,n,nw,n,s,se,ne,nw,nw,sw,nw,nw,ne,n,n,n,nw,n,nw,nw,s,nw,n,nw,se,n,n,n,n,nw,nw,n,s,n,n,n,se,n,sw,se,se,n,n,n,sw,n,sw,nw,sw,n,nw,n,n,ne,se,n,n,n,n,ne,se,s,s,nw,n,sw,n,se,n,ne,nw,nw,n,nw,ne,nw,nw,nw,n,n,ne,nw,n,n,se,n,sw,n,n,nw,s,n,n,nw,nw,n,nw,nw,n,se,n,nw,nw,n,n,n,ne,nw,n,nw,n,n,nw,s,ne,n,nw,nw,se,n,s,nw,s,nw,nw,n,nw,n,nw,nw,nw,nw,nw,n,n,sw,nw,s,nw,n,nw,nw,n,sw,n,n,n,sw,s,nw,nw,nw,nw,nw,nw,nw,ne,nw,nw,nw,nw,s,se,nw,nw,nw,nw,nw,ne,nw,nw,n,n,n,n,nw,n,nw,n,n,n,nw,n,n,nw,nw,nw,se,nw,nw,s,nw,nw,nw,sw,ne,se,nw,n,sw,nw,nw,nw,nw,n,nw,n,nw,n,nw,n,n,nw,nw,n,nw,nw,nw,nw,nw,nw,n,se,n,n,n,sw,nw,nw,se,se,nw,ne,n,nw,se,nw,nw,s,nw,nw,n,nw,nw,nw,nw,n,nw,n,nw,sw,nw,nw,nw,nw,nw,nw,nw,nw,sw,nw,ne,nw,n,nw,nw,nw,n,n,sw,sw,nw,s,nw,nw,nw,nw,n,s,n,nw,nw,n,nw,nw,nw,nw,nw,ne,sw,nw,nw,nw,nw,n,nw,nw,nw,n,ne,s,nw,nw,nw,nw,nw,nw,nw,nw,nw,ne,sw,n,se,nw,nw,nw,nw,nw,nw,n,nw,s,n,nw,nw,n,s,nw,nw,nw,nw,nw,nw,nw,se,nw,nw,nw,nw,nw,n,nw,n,n,nw,nw,nw,nw,nw,nw,nw,nw,n,n,nw,n,sw,nw,nw,nw,nw,se,nw,n,nw,nw,nw,nw,se,nw,ne,nw,nw,n,nw,nw,nw,nw,nw,ne,nw,n,nw,s,se,sw,se,nw,nw,nw,nw,se,sw,n,ne,ne,nw,se,s,ne,nw,s,nw,nw,nw,nw,nw,nw,s,nw,nw,n,n,ne,nw,nw,nw,nw,nw,nw,ne,ne,nw,nw,s,se,nw,nw,nw,n,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,s,nw,nw,nw,nw,sw,nw,sw,n,nw,nw,nw,se,nw,n,nw,nw,se,se,nw,sw,nw,s,nw,n,nw,nw,nw,se,nw,sw,s,nw,s,nw,nw,nw,nw,nw,ne,nw,nw,nw,nw,nw,nw,nw,s,sw,nw,se,s,nw,nw,se,nw,nw,nw,nw,n,nw,s,nw,nw,nw,nw,sw,sw,nw,nw,sw,nw,nw,nw,sw,nw,nw,ne,nw,nw,nw,nw,nw,nw,sw,sw,nw,nw,nw,sw,sw,se,n,ne,nw,nw,sw,s,sw,nw,s,ne,nw,sw,sw,nw,se,n,nw,nw,nw,nw,nw,nw,s,nw,se,se,ne,nw,nw,nw,ne,sw,nw,nw,nw,nw,se,nw,sw,nw,nw,nw,sw,nw,sw,nw,ne,sw,nw,nw,nw,nw,s,nw,nw,nw,se,nw,s,sw,nw,nw,nw,sw,nw,nw,nw,nw,sw,nw,nw,se,ne,sw,sw,sw,nw,se,nw,nw,sw,nw,nw,nw,nw,nw,nw,nw,s,ne,nw,nw,nw,sw,nw,nw,sw,s,sw,nw,sw,nw,nw,nw,nw,nw,sw,nw,se,sw,nw,nw,sw,se,nw,nw,nw,se,n,nw,nw,n,sw,nw,nw,nw,sw,se,nw,nw,nw,nw,sw,ne,nw,s,nw,se,sw,s,sw,sw,sw,n,nw,sw,n,nw,nw,se,nw,n,ne,se,sw,nw,nw,nw,nw,nw,se,se,nw,nw,nw,n,nw,n,nw,nw,nw,nw,sw,nw,sw,nw,n,sw,sw,nw,nw,se,ne,nw,sw,nw,sw,sw,nw,nw,sw,sw,se,n,sw,nw,sw,nw,sw,nw,nw,nw,nw,sw,se,nw,nw,sw,nw,sw,nw,nw,nw,ne,nw,sw,se,nw,s,nw,ne,nw,sw,nw,sw,nw,nw,nw,ne,sw,nw,nw,sw,nw,sw,nw,sw,nw,sw,sw,sw,sw,nw,sw,ne,nw,sw,nw,nw,nw,nw,nw,sw,nw,nw,sw,s,sw,nw,nw,nw,nw,sw,sw,s,nw,se,se,sw,sw,nw,nw,sw,ne,nw,sw,sw,sw,sw,sw,sw,nw,nw,n,sw,sw,nw,nw,n,nw,n,sw,sw,sw,sw,se,sw,nw,sw,sw,sw,nw,sw,nw,se,sw,nw,sw,sw,sw,nw,sw,sw,ne,sw,nw,s,se,nw,sw,nw,nw,sw,sw,nw,nw,nw,nw,sw,se,sw,n,sw,sw,sw,s,sw,se,se,nw,sw,sw,sw,nw,sw,sw,ne,nw,nw,sw,se,sw,s,sw,nw,n,nw,nw,sw,s,nw,sw,sw,sw,nw,sw,nw,ne,ne,sw,nw,nw,sw,sw,nw,ne,nw,sw,sw,ne,sw,nw,nw,nw,sw,sw,nw,s,nw,sw,nw,sw,sw,s,n,nw,sw,s,ne,sw,sw,sw,ne,ne,sw,sw,sw,sw,se,nw,sw,s,nw,se,sw,n,nw,s,sw,sw,sw,sw,sw,sw,se,sw,nw,sw,nw,se,sw,s,sw,nw,sw,ne,sw,sw,sw,sw,sw,sw,nw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,n,nw,n,sw,ne,nw,sw,n,sw,sw,sw,nw,sw,sw,nw,n,sw,nw,sw,sw,sw,sw,se,sw,sw,sw,nw,sw,nw,sw,nw,sw,sw,sw,nw,sw,sw,sw,sw,nw,sw,s,nw,sw,sw,sw,sw,sw,nw,sw,sw,sw,sw,nw,sw,sw,sw,nw,se,sw,se,sw,n,sw,sw,sw,sw,se,se,sw,nw,sw,sw,n,sw,s,sw,nw,sw,sw,sw,sw,sw,sw,sw,s,sw,nw,se,sw,sw,sw,se,sw,sw,sw,sw,sw,sw,sw,sw,nw,sw,sw,sw,s,sw,sw,nw,sw,sw,sw,nw,sw,sw,s,se,ne,sw,sw,s,nw,sw,sw,sw,sw,sw,s,sw,se,sw,s,s,sw,sw,ne,sw,sw,sw,sw,sw,sw,se,sw,sw,sw,n,sw,sw,sw,sw,ne,sw,sw,ne,se,sw,sw,sw,sw,sw,n,n,sw,nw,sw,s,sw,sw,sw,sw,ne,sw,sw,sw,ne,ne,nw,ne,sw,se,sw,n,sw,sw,sw,sw,sw,s,sw,sw,sw,sw,ne,sw,sw,sw,s,sw,sw,sw,sw,sw,n,sw,sw,sw,nw,n,sw,sw,sw,sw,sw,ne,sw,sw,n,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,nw,sw,sw,sw,sw,ne,sw,nw,sw,n,sw,sw,sw,sw,sw,n,sw,sw,sw,se,sw,sw,sw,sw,n,sw,nw,nw,sw,sw,sw,se,sw,sw,sw,s,sw,sw,nw,sw,sw,sw,nw,s,sw,n,s,s,sw,sw,sw,ne,sw,sw,sw,sw,se,s,ne,sw,sw,n,s,sw,sw,sw,se,sw,sw,se,sw,sw,ne,sw,nw,sw,sw,sw,sw,sw,sw,sw,sw,sw,s,sw,sw,sw,n,sw,sw,sw,nw,s,sw,s,se,sw,sw,sw,s,sw,s,sw,nw,s,sw,s,sw,se,n,sw,sw,sw,sw,sw,s,n,s,s,ne,nw,sw,sw,sw,se,sw,sw,sw,sw,sw,sw,n,sw,se,sw,nw,sw,sw,sw,s,n,sw,sw,n,sw,nw,sw,sw,sw,sw,sw,sw,s,nw,s,sw,sw,sw,s,n,sw,sw,sw,sw,sw,sw,ne,sw,n,sw,s,sw,s,sw,n,s,ne,sw,sw,s,s,sw,sw,nw,s,n,sw,se,sw,nw,sw,sw,sw,sw,n,sw,sw,sw,sw,ne,sw,sw,sw,sw,se,sw,sw,sw,sw,sw,n,sw,sw,s,sw,sw,sw,sw,sw,sw,sw,sw,sw,se,sw,ne,sw,sw,s,sw,sw,s,sw,sw,sw,sw,sw,s,se,nw,sw,n,sw,sw,sw,sw,sw,sw,n,sw,se,sw,s,sw,sw,ne,n,sw,n,sw,sw,sw,sw,sw,ne,s,sw,s,nw,s,sw,sw,sw,nw,s,sw,sw,sw,s,sw,s,nw,sw,sw,s,sw,sw,sw,sw,sw,n,sw,s,n,sw,s,n,s,sw,sw,sw,s,sw,sw,sw,s,n,s,sw,sw,s,sw,sw,sw,sw,s,se,se,s,sw,s,sw,sw,sw,sw,sw,sw,se,nw,sw,sw,s,nw,s,nw,sw,ne,sw,n,sw,sw,sw,s,ne,sw,s,sw,nw,sw,s,sw,s,sw,sw,ne,sw,sw,ne,s,sw,sw,ne,sw,se,nw,sw,s,s,s,sw,s,nw,nw,ne,sw,sw,sw,sw,se,sw,sw,sw,s,ne,sw,sw,sw,sw,sw,sw,sw,s,sw,se,s,sw,s,sw,sw,s,sw,s,sw,s,sw,sw,sw,ne,s,ne,s,ne,s,s,ne,nw,sw,s,sw,n,s,s,sw,sw,n,s,sw,sw,s,s,s,s,s,sw,sw,s,sw,sw,sw,sw,ne,s,sw,s,nw,sw,sw,sw,sw,sw,n,sw,sw,s,se,sw,s,sw,ne,sw,sw,se,sw,s,s,sw,se,sw,sw,n,sw,sw,sw,s,s,s,n,s,se,sw,sw,s,s,sw,sw,s,s,sw,sw,sw,sw,sw,s,se,sw,sw,n,sw,s,s,s,sw,s,s,sw,se,s,s,se,sw,sw,se,se,sw,s,n,n,n,s,s,se,s,nw,sw,sw,sw,s,sw,sw,sw,s,sw,sw,s,sw,sw,s,s,n,sw,s,sw,s,s,s,se,s,s,s,sw,s,sw,se,sw,sw,sw,sw,s,sw,s,n,se,s,s,n,sw,sw,se,s,sw,s,n,s,s,sw,nw,sw,sw,sw,n,sw,s,n,s,sw,sw,sw,s,n,sw,sw,sw,sw,s,sw,s,nw,s,s,sw,s,s,n,s,sw,s,n,n,sw,se,sw,s,sw,s,s,sw,sw,sw,s,sw,nw,nw,se,sw,sw,s,s,s,sw,s,s,sw,ne,s,sw,sw,s,sw,n,s,s,s,sw,s,sw,s,s,s,sw,sw,sw,sw,sw,s,s,sw,sw,sw,se,sw,sw,s,sw,n,sw,s,s,s,nw,s,s,sw,s,s,s,s,sw,se,sw,sw,s,sw,n,sw,s,sw,se,s,s,s,sw,sw,n,s,sw,s,s,sw,s,s,s,sw,ne,sw,s,sw,s,ne,n,s,s,sw,sw,sw,s,s,sw,sw,s,sw,s,sw,sw,sw,sw,s,s,sw,n,s,n,s,sw,se,s,sw,sw,s,sw,s,s,s,sw,s,s,s,s,s,s,nw,sw,s,sw,sw,s,s,sw,sw,sw,s,s,sw,sw,s,s,s,sw,s,s,se,se,s,se,s,s,s,s,s,nw,s,n,sw,sw,s,ne,s,s,s,s,sw,s,sw,se,se,sw,sw,sw,nw,s,s,s,ne,sw,s,s,s,s,n,s,s,sw,s,sw,s,n,s,sw,se,s,nw,s,n,s,s,s,n,s,nw,s,s,s,n,s,s,s,sw,ne,s,s,s,s,s,sw,s,ne,sw,ne,s,s,s,sw,sw,sw,s,ne,nw,ne,nw,s,sw,s,s,s,s,s,s,s,s,n,sw,s,s,s,s,ne,s,s,s,s,s,s,s,s,sw,s,se,s,s,s,s,s,s,sw,se,n,se,n,s,s,s,s,s,s,s,nw,sw,n,s,s,s,s,s,s,sw,sw,s,s,sw,s,nw,s,s,nw,s,s,s,sw,s,s,n,s,s,s,s,sw,s,s,s,sw,s,s,s,ne,se,sw,s,s,n,s,s,se,sw,sw,s,s,ne,sw,s,s,s,sw,n,s,nw,s,n,s,s,s,s,se,s,s,sw,nw,s,s,se,sw,s,s,s,s,se,sw,s,s,s,s,s,ne,sw,s,s,s,nw,s,s,s,ne,s,s,n,s,sw,s,s,s,s,s,ne,s,sw,nw,s,s,s,n,s,n,sw,s,n,s,s,s,s,s,s,n,s,nw,sw,s,nw,s,nw,s,s,s,s,s,sw,n,s,s,s,s,s,s,s,s,s,sw,s,s,s,s,s,s,s,s,s,n,s,s,s,s,s,n,s,s,s,s,s,s,s,n,nw,ne,s,s,nw,s,se,s,s,sw,s,s,s,s,n,s,s,s,s,s,sw,s,s,s,s,s,ne,s,s,s,s,s,s,s,s,s,n,s,s,s,s,s,s,s,s,s,n,s,nw,s,s,ne,s,s,ne,s,s,s,n,s,s,s,s,se,s,s,s,s,s,s,s,n,ne,s,s,s,s,se,s,n,s,sw,s,s,s,s,s,s,nw,ne,s,nw,s,s,s,ne,s,s,ne,s,s,s,s,s,sw,s,s,s,s,n,s,s,s,s,s,s,s,s,se,s,n,n,s,s,s,s,s,s,se,s,s,s,s,s,sw,n,ne,s,s,s,se,s,s,nw,s,n,s,ne,s,s,ne,se,s,s,s,s,s,s,n,n,s,nw,s,s,s,s,s,s,s,s,nw,se,s,s,se,s,ne,s,se,nw,s,nw,s,s,s,sw,nw,s,s,s,s,se,s,n,se,se,s,s,s,nw,s,s,nw,ne,ne,s,s,nw,s,s,s,se,s,se,s,ne,se,s,s,s,sw,s,s,ne,s,s,s,s,s,s,s,se,s,se,s,s,ne,s,s,s,s,s,se,s,s,nw,s,s,se,se,ne,se,se,s,s,s,s,s,s,s,se,nw,s,nw,s,s,s,n,s,s,s,s,s,s,s,s,s,s,s,nw,s,s,s,sw,s,n,s,se,s,s,s,s,s,ne,s,s,s,s,s,se,n,s,s,s,se,ne,s,s,s,s,s,sw,s,se,s,s,se,s,s,se,s,se,se,s,se,s,s,s,se,s,s,s,sw,se,s,s,s,s,s,s,s,s,nw,s,s,s,s,se,s,s,s,s,sw,ne,s,s,sw,s,s,s,se,n,s,s,sw,se,s,se,s,nw,s,s,se,s,s,s,s,n,s,nw,sw,s,s,s,n,s,s,s,s,s,s,se,s,se,se,s,s,s,s,sw,s,s,se,s,s,s,se,s,s,s,ne,se,s,s,s,nw,s,s,s,s,n,se,s,s,s,s,se,s,se,s,sw,s,ne,s,s,sw,s,sw,se,se,s,s,s,s,se,s,nw,nw,s,n,s,s,s,s,s,nw,s,s,n,s,s,sw,s,se,se,s,sw,s,s,n,s,s,s,s,se,nw,s,se,nw,s,s,s,se,se,s,se,s,ne,s,nw,sw,s,s,s,nw,s,se,s,s,s,se,s,s,s,se,se,sw,s,s,sw,s,se,s,s,s,se,sw,s,se,se,s,s,s,s,se,s,s,s,sw,se,ne,s,se,s,se,s,s,s,ne,s,sw,sw,s,s,se,s,se,se,se,s,s,s,se,s,s,sw,nw,se,nw,s,s,s,s,s,s,s,se,s,se,s,sw,se,se,s,se,s,sw,s,se,s,se,s,se,s,s,sw,nw,se,sw,s,se,s,nw,s,s,s,s,s,s,nw,n,s,se,s,se,sw,s,se,se,ne,nw,se,ne,se,se,se,s,s,s,se,sw,s,sw,se,se,se,s,se,se,se,s,s,se,sw,se,se,s,s,se,sw,s,se,s,s,se,se,s,sw,se,n,se,se,s,sw,s,se,ne,s,s,s,s,s,se,s,s,se,s,s,s,se,se,sw,s,se,s,s,n,s,se,s,se,s,n,sw,se,se,se,s,s,s,se,n,sw,s,se,sw,se,se,s,se,se,se,s,s,sw,s,s,nw,se,se,se,s,se,s,s,nw,s,s,s,s,se,s,s,s,se,s,ne,nw,s,s,se,s,sw,se,se,s,s,s,se,s,se,s,se,se,sw,n,se,se,se,s,ne,se,nw,s,s,se,s,s,se,s,s,se,se,se,s,s,ne,se,s,sw,n,s,se,se,se,s,s,s,s,se,se,se,s,se,s,nw,se,s,s,s,se,se,se,s,s,s,se,se,s,se,s,se,nw,s,s,se,s,sw,se,se,se,s,se,sw,s,se,s,s,s,s,s,se,s,se,s,se,se,se,s,sw,nw,nw,s,nw,sw,se,s,s,ne,nw,se,se,se,s,n,se,s,s,se,s,ne,se,ne,se,s,s,s,s,se,se,se,se,s,se,nw,se,s,s,ne,s,s,s,ne,se,s,sw,s,s,sw,s,se,se,se,se,sw,s,se,se,s,nw,se,s,s,se,ne,s,sw,s,s,nw,se,n,s,s,ne,se,n,ne,se,s,ne,se,s,s,ne,se,ne,n,s,se,nw,se,se,ne,s,s,s,s,ne,se,s,se,n,se,se,s,s,ne,se,s,se,se,se,s,se,nw,se,sw,se,s,s,se,sw,se,se,s,ne,se,s,ne,s,se,s,se,s,sw,se,s,s,se,s,sw,se,se,s,s,sw,s,se,s,sw,s,se,se,s,se,s,se,nw,sw,se,ne,se,se,se,s,se,se,se,nw,se,se,se,se,se,se,sw,se,se,s,se,sw,nw,se,se,s,se,s,s,se,se,s,se,se,s,nw,s,s,se,se,se,s,se,se,s,sw,sw,n,s,se,se,s,s,s,s,s,se,sw,s,se,s,nw,sw,se,sw,ne,s,se,se,se,se,n,s,se,se,se,s,s,se,s,se,s,ne,sw,se,se,sw,se,s,se,se,se,s,s,se,s,se,nw,se,s,n,se,s,se,nw,s,s,se,se,se,s,se,s,se,se,sw,se,s,sw,s,se,se,se,s,se,s,s,s,se,se,s,nw,se,nw,n,sw,s,s,s,se,se,s,se,se,se,se,s,s,ne,se,s,se,nw,se,se,ne,s,se,se,s,s,se,se,se,s,se,s,s,s,se,se,ne,s,ne,s,se,se,se,se,sw,se,se,se,se,se,se,se,sw,se,s,s,se,s,n,n,ne,se,s,ne,se,se,s,sw,se,s,nw,s,nw,se,s,s,se,se,se,se,nw,nw,se,se,se,s,n,se,se,se,ne,ne,n,se,se,se,s,se,n,s,s,ne,s,s,n,se,se,s,ne,se,se,s,nw,n,se,se,se,se,sw,se,s,s,se,se,se,se,n,n,se,se,s,s,se,nw,se,n,ne,s,se,nw,se,ne,se,se,ne,se,sw,s,s,se,se,s,se,s,se,se,se,n,s,se,se,s,s,n,s,s,se,se,se,se,se,sw,n,se,se,se,se,se,s,s,ne,s,n,s,se,s,se,se,se,se,se,ne,se,se,se,s,sw,se,ne,se,se,se,s,se,se,se,sw,se,s,ne,se,se,se,se,s,nw,ne,s,s,se,s,se,se,se,se,se,se,se,s,se,se,se,nw,s,se,sw,se,ne,ne,se,se,se,s,se,se,se,se,se,n,se,se,se,se,se,sw,nw,se,se,se,se,s,se,se,se,ne,s,ne,se,se,se,s,se,se,se,se,s,se,sw,se,se,se,se,se,sw,sw,se,se,s,se,se,se,se,n,se,se,se,se,se,se,ne,se,se,se,se,se,se,nw,se,se,se,s,se,se,se,se,se,se,se,se,se,se,se,se,s,se,s,s,se,ne,se,se,nw,nw,se,se,se,s,s,sw,s,se,se,se,se,se,se,se,se,se,se,se,n,se,se,nw,se,se,s,se,se,se,se,s,se,se,se,se,se,se,sw,se,se,se,se,se,se,se,n,se,n,se,n,nw,sw,se,sw,se,se,se,se,se,se,se,se,nw,se,se,se,se,se,se,se,se,ne,se,se,nw,ne,se,se,se,se,nw,sw,se,se,se,n,se,ne,se,ne,se,se,s,se,s,se,se,se,ne,ne,se,se,se,se,se,se,se,se,n,se,se,se,se,nw,se,se,sw,n,s,se,n,se,s,se,nw,sw,se,se,se,se,se,se,nw,se,se,se,sw,se,se,se,se,se,se,se,se,se,se,se,se,se,n,s,se,se,se,se,se,nw,n,se,se,nw,nw,se,se,se,se,n,se,se,se,se,se,se,s,sw,n,nw,se,nw,nw,n,n,n,se,n,n,n,n,s,se,n,ne,nw,ne,se,se,ne,ne,ne,se,nw,ne,sw,se,se,se,se,se,se,se,se,s,se,s,se,s,ne,se,s,sw,se,se,se,s,se,s,s,se,se,s,s,s,s,s,n,s,s,s,s,s,s,s,s,s,s,sw,s,n,s,sw,s,s,s,se,se,s,s,sw,sw,sw,sw,s,sw,ne,s,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,nw,sw,sw,sw,sw,nw,sw,nw,sw,sw,ne,nw,sw,nw,nw,se,nw,s,sw,nw,nw,sw,sw,se,sw,nw,nw,nw,s,sw,nw,ne,nw,nw,nw,nw,ne,nw,nw,sw,se,nw,nw,ne,nw,nw,nw,nw,n,nw,nw,nw,nw,nw,nw,nw,n,n,nw,nw,nw,nw,nw,se,nw,nw,nw,nw,nw,sw,se,n,n,n,nw,n,nw,ne,n,nw,nw,nw,n,nw,nw,n,n,nw,nw,n,n,n,n,sw,se,n,se,n,n,n,se,n,n,n,n,se,n,n,n,n,n,sw,n,n,n,sw,s,n,n,n,n,n,n,n,n,n,se,s,n,n,nw,ne,sw,s,n,s,s,s,n,n,n,ne,ne,n,n,n,n,n,n,n,ne,ne,n,n,ne,ne,n,n,ne,n,ne,ne,n,n,ne,ne,sw,ne,nw,ne,s,ne,sw,n,ne,n,se,ne,ne,se,nw,se,ne,sw,ne,ne,ne,ne,ne,s,ne,ne,ne,n,ne,n,ne,ne,ne,ne,ne,ne,ne,ne,se,se,ne,se,sw,ne,ne,ne,ne,ne,ne,ne,se,ne,ne,sw,ne,ne,ne,sw,ne,ne,ne,ne,ne,n,se,ne,ne,se,ne,nw,s,sw,ne,ne,ne,ne,n,n,ne,ne,se,ne,ne,ne,se,ne,ne,sw,n,sw,ne,ne,ne,ne,ne,se,nw,s,se,se,ne,ne,s,nw,ne,ne,ne,n,se,ne,ne,se,ne,ne,ne,ne,se,se,ne,ne,s,ne,ne,ne,ne,ne,se,ne,se,se,ne,ne,se,sw,ne,se,ne,ne,ne,ne,se,nw,ne,se,s,se,ne,se,sw,se,se,se,nw,se,se,ne,se,se,se,se,se,ne,ne,se,sw,ne,se,ne,ne,se,se,nw,se,se,ne,ne,se,se,se,se,se,ne,se,se,se,se,se,se,se,ne,se,se,sw,ne,s,se,s,se,se,nw,se,se,s,se,se,se,se,se,s,se,se,se,se,se,n,se,se,se,se,s,se,sw,se,se,n,se,se,se,ne,se,nw,n,nw,s,se,se,ne,s,s,se,se,s,se,n,se,sw,se,se,s,s,n,se,s,se,ne,se,se,se,se,s,se,se,sw,sw,s,se,s,s,se,s,se,s,nw,n,s,s,s,nw,se,nw,nw,ne,ne,ne,s,s,se,se,n,se,s,se,se,se,s,s,se,s,se,se,se,s,nw,ne,se,se,s,se,sw,sw,s,s,s,se,se,se,s,se,s,s,nw,se,se,se,se,ne,sw,s,se,s,s,nw,s,s,s,ne,se,nw,s,s,se,s,s,s,s,s,se,s,s,se,se,s,se,se,se,se,s,s,s,s,se,se,se,s,s,sw,se,se,s,ne,sw,n,s,s,n,s,se,s,s,s,nw,se,nw,s,s,s,s,s,sw,s,s,s,s,s,s,sw,n,ne,nw,ne,s,s,s,s,s,sw,s,s,s,s,ne,s,s,s,s,s,se,ne,se,s,s,s,sw,se,s,s,sw,sw,s,s,s,s,sw,sw,s,n,n,s,se,nw,nw,s,sw,nw,sw,s,s,ne,s,s,nw,s,s,s,s,sw,sw,n,nw,n,s,s,s,s,s,s,nw,s,s,sw,nw,s,s,n,s,s,s,s,s,s,sw,s,s,s,s,sw,s,s,s,s,s,se,n,s,sw,s,sw,s,s,nw,se,sw,s,s,sw,n,s,sw,se,s,s,s,s,s,s,sw,sw,s,sw,n,sw,se,s,sw,sw,s,s,nw,s,s,sw,s,ne,s,sw,se,sw,s,s,sw,se,n,sw,s,sw,ne,s,sw,s,s,s,sw,nw,s,s,n,sw,sw,sw,s,se,s,nw,s,s,s,sw,sw,s,ne,ne,sw,sw,s,sw,s,sw,s,sw,n,n,sw,n,sw,s,se,sw,sw,sw,ne,s,sw,s,sw,sw,sw,sw,ne,sw,sw,nw,nw,s,n,s,se,s,sw,sw,se,ne,ne,sw,sw,sw,s,n,s,sw,sw,sw,sw,s,sw,sw,s,s,sw,nw,s,s,sw,sw,s,sw,sw,sw,s,sw,sw,sw,sw,sw,ne,se,sw,s,sw,ne,sw,ne,s,n,sw,sw,s,sw,ne,s,sw,ne,sw,s,sw,sw,sw,s,ne,sw,s,s,sw,sw,sw,n,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,nw,s,sw,sw,sw,ne,sw,nw,nw,sw,sw,sw,sw,sw,s,sw,n,sw,nw,nw,s,sw,sw,s,sw,sw,sw,ne,sw,nw,sw,sw,sw,nw,sw,n,nw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,s,sw,sw,sw,sw,se,nw,ne,sw,n,sw,sw,sw,s,sw,se,sw,nw,sw,n,sw,se,sw,sw,s,sw,nw,sw,sw,sw,sw,sw,nw,sw,sw,sw,sw,sw,sw,sw,sw,nw,sw,sw,sw,s,sw,ne,sw,sw,nw,sw,sw,sw,sw,sw,s,sw,sw,sw,n,sw,nw,se,sw,sw,sw,sw,nw,se,nw,nw,sw,sw,sw,sw,nw,ne,nw,sw,nw,se,n,sw,sw,sw,sw,sw,nw,sw,sw,se,sw,nw,sw,nw,n,s,s,nw,nw,sw,sw,nw,n,sw,nw,sw,nw,s,ne,nw,n,nw,nw,sw,sw,n,n,sw,nw,sw,se,nw,nw,sw,nw,n,sw,sw,sw,nw,nw,nw,sw,n,nw,sw,se,sw,nw,se,nw,sw,nw,nw,nw,ne,nw,nw,nw,sw,se,n,sw,nw,nw,sw,nw,sw,sw,sw,sw,sw,sw,sw,s,sw,sw,nw,sw,nw,nw,ne,sw,nw,n,sw,sw,nw,nw,sw,n,sw,sw,nw,sw,nw,nw,sw,nw,nw,sw,ne,nw,sw,nw,n,nw,nw,nw,nw,s,nw,se,nw,sw,sw,ne,nw,nw,nw,sw,se,nw,nw,nw,sw,nw,n,sw,nw,nw,nw,sw,sw,nw,ne,nw,ne,se,nw,ne,nw,nw,nw,sw,nw,nw,sw,se,nw,nw,nw,sw,ne,sw,nw,nw,nw,sw,nw,nw,sw,ne,nw,nw,nw,nw,nw,ne,nw,nw,nw,nw,nw,nw,nw,nw,ne,nw,sw,nw,se,ne,nw,nw,sw,nw,nw,nw,sw,sw,nw,sw,sw,se,ne,nw,sw,nw,nw,n,nw,nw,se,nw,se,nw,nw,nw,ne,nw,nw,nw,nw,ne,ne,nw,sw,se,nw,nw,nw,sw,nw,nw,nw,nw,sw,ne,nw,nw,s,ne,nw,nw,nw,nw,nw,nw,nw,n,nw,nw,nw,s,nw,n,sw,ne,ne,nw,nw,s,nw,se,nw,se,nw,nw,nw,nw,nw,n,nw,nw,s,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,ne,nw,nw,nw,nw,sw,nw,nw,ne,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,ne,nw,n,nw,nw,nw,nw,nw,nw,nw,nw,nw,ne,sw,nw,ne,nw,s,nw,nw,nw,n,nw,nw,nw,nw,nw,nw,se,nw,nw,n,nw,se,nw,ne,se,ne,s,nw,se,nw,nw,n,n,nw,nw,n,nw,nw,nw,nw,se,nw,ne,n,nw,nw,nw,n,nw,sw,nw,s,nw,nw,n,nw,nw,n,nw,nw,ne,sw,nw,sw,nw,ne,nw,nw,se,nw,nw,n,n,nw,n,nw,n,s,nw,nw,nw,nw,se,nw,sw,nw,nw,nw,n,ne,se,n,nw,n,nw,nw,n,nw,n,n,nw,nw,nw,nw,n,nw,nw,nw,nw,n,nw,nw,nw,n,s,sw,sw,nw,nw,nw,nw,nw,nw,nw,sw,nw,nw,nw,nw,nw,nw,n,nw,nw,nw,n,nw,nw,s,nw,nw,n,nw,ne,ne,n,n,n,n,nw,n,n,nw,n,ne,n,nw,n,n,n,se,nw,se,n,nw,nw,nw,n,nw,s,n,nw,nw,nw,n,nw,nw,nw,nw,n,n,n,nw,n,s,se,n,nw,nw,nw,nw,n,nw,nw,nw,nw,n,ne,n,n,nw,s,nw,nw,nw,n,s,ne,n,nw,nw,nw,n,s,nw,se,sw,se,nw,n,n,nw,nw,nw,nw,n,nw,nw,nw,n,n,n,nw,ne,nw,n,nw,se,ne,s,nw,nw,n,nw,nw,s,nw,s,s,n,sw,nw,n,s,n,n,ne,se,nw,n,nw,nw,se,n,nw,nw,nw,n,nw,n,nw,nw,s,n,nw,nw,n,ne,n,ne,n,n,nw,se,sw,n,n,nw,nw,n,nw,ne,nw,n,n,nw,nw,n,n,n,nw,nw,nw,se,n,nw,sw,nw,n,n,se,n,nw,nw,nw,n,nw,nw,n,nw,nw,nw,nw,n,n,se,ne,n,n,nw,se,nw,n,n,n,se,nw,n,n,n,n,n,n,ne,n,nw,n,se,n,nw,nw,n,n,n,nw,n,n,s,nw,nw,n,n,n,n,n,nw,nw,n,sw,n,sw,s,n,n,n,n,n,n,n,n,nw,n,n,n,n,se,ne,n,s,n,n,nw,nw,n,nw,nw,n,n,nw,n,n,n,sw,nw,nw,se,n,n,nw,n,n,n,n,n,n,n,n,nw,sw,n,nw,n,nw,n,n,n,n,n,s,nw,nw,ne,n,s,ne,nw,n,n,n,n,s,nw,ne,n,n,n,nw,n,n,n,n,s,s,n,n,n,n,n,n,n,n,n,n,n,n,n,n,s,s,n,n,se,n,n,ne,n,n,n,n,n,n,n,n,n,n,n,nw,s,n,n,n,n,n,n,n,n,nw,n,n,nw,se,n,n,n,n,n,nw,n,n,n,n,n,n,n,sw,se,n,n,se,n,n,n,sw,n,n,n,s,n,n,sw,n,n,n,n,n,n,n,n,n,n,n,n,n,se,n,n,n,sw,n,ne,nw,s,n,nw,n,n,ne,se,ne,n,n,n,n,s,n,n,n,n,n,s,n,n,n,n,n,n,nw,n,sw,n,n,se,nw,n,n,n,n,se,n,n,n,n,n,n,nw,se,n,n,sw,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,se,nw,s,s,n,n,n,n,n,ne,s,n,n,sw,n,n,n,n,n,ne,n,n,n,n,ne,ne,sw,s,ne,n,ne,s,n,s,n,n,n,se,n,n,ne,ne,n,n,n,n,n,n,n,se,ne,s,ne,n,n,ne,n,ne,ne,n,n,se,n,ne,sw,n,n,n,n,n,n,n,n,n,n,n,ne,n,n,n,sw,n,n,s,se,sw,n,ne,ne,n,se,ne,n,n,n,n,n,n,ne,n,n,ne,n,ne,ne,ne,n,ne,n,s,n,se,nw,ne,nw,n,se,n,n,nw,n,ne,n,n,ne,n,ne,s,s,sw,nw,n,s,ne,n,n,se,se,n,n,n,n,ne,n,n,ne,n,n,ne,ne,ne,n,n,n,n,ne,ne,n,n,s,n,n,ne,n,n,n,ne,ne,n,n,ne,ne,n,n,n,ne,ne,n,se,ne,ne,ne,n,n,sw,ne,ne,ne,n,n,n,ne,n,n,n,sw,se,se,n,n,n,ne,n,n,n,ne,ne,n,se,s,n,n,n,ne,n,sw,ne,ne,ne,n,ne,n,n,n,ne,n,ne,n,se,n,ne,ne,se,n,n,n,n,n,n,n,se,n,n,n,n,s,n,ne,ne,nw,ne,ne,n,nw,n,n,n,n,n,n,n,n,ne,n,s,n,ne,ne,n,ne,ne,s,n,ne,n,ne,n,ne,ne,ne,ne,sw,n,ne,s,ne,ne,ne,se,ne,ne,ne,ne,n,ne,ne,ne,se,n,ne,ne,n,ne,n,ne,ne,se,n,ne,n,n,n,n,ne,ne,ne,n,n,ne,ne,ne,ne,sw,ne,sw,nw,nw,s,n,ne,se,ne,ne,n,n,ne,n,ne,ne,s,n,ne,sw,ne,n,n,ne,se,n,ne,sw,n,nw,n,n,ne,ne,s,ne,n,ne,sw,ne,n,ne,n,n,n,n,ne,sw,ne,se,n,n,sw,n,s,nw,n,ne,ne,ne,ne,n,n,ne,ne,sw,n,n,ne,ne,nw,ne,n,nw,nw,ne,n,ne,ne,ne,ne,n,n,ne,ne,ne,s,n,nw,ne,ne,ne,nw,nw,n,se,nw,ne,ne,ne,ne,n,n,ne,n,ne,nw,n,ne,s,ne,n,s,ne,ne,nw,ne,n,ne,nw,se,n,n,n,n,ne,s,ne,n,n,ne,ne,n,ne,ne,n,ne,ne,se,n,ne,n,ne,n,nw,ne,n,n,ne,ne,ne,n,ne,n,s,ne,ne,ne,n,ne,n,ne,s,n,n,ne,n,ne,ne,ne,n,n,ne,n,ne,ne,s,ne,n,n,ne,ne,ne,n,ne,ne,n,ne,nw,ne,ne,ne,s,ne,s,ne,se,n,ne,sw,ne,ne,n,ne,ne,ne,ne,ne,sw,ne,s,n,n,sw,s,ne,n,ne,n,ne,ne,ne,ne,nw,ne,ne,n,ne,ne,nw,se,ne,ne,nw,s,ne,ne,ne,ne,ne,ne,s,ne,ne,ne,ne,ne,ne,sw,ne,n,ne,ne,ne,ne,n,ne,ne,ne,se,ne,se,ne,n,n,ne,ne,ne,n,ne,sw,n,ne,n,ne,ne,n,n,n,se,ne,n,sw,se,s,sw,ne,ne,n,n,n,sw,ne,ne,s,ne,ne,n,n,ne,ne,ne,nw,ne,ne,s,ne,ne,ne,ne,ne,n,ne,ne,n,ne,ne,nw,ne,se,n,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,n,ne,ne,ne,ne,ne,ne,ne,s,sw,ne,n,se,ne,ne,sw,ne,nw,ne,ne,nw,n,n,ne,ne,ne,ne,ne,ne,nw,ne,sw,ne,ne,ne,nw,ne,ne,ne,ne,ne,ne,ne,nw,ne,ne,ne,nw,ne,nw,nw,ne,nw,ne,ne,ne,n,se,n,sw,ne,ne,ne,ne,ne,ne,ne,se,sw,ne"

parse :: String -> [String]
parse = splitOn ","

walk :: [String] -> (Int, Int)
walk = foldl step (0, 0)

step :: (Int, Int) -> String -> (Int, Int)
step (x, y) d = let (x', y') = direction d
                in (x + x', y + y')

-- NW|N |
-- --+--+--
-- SW|  |NE
-- --+--+--
--   |S |SE

direction :: String -> (Int, Int)
direction "nw" = (-1, 1)
direction "n" = (0, 1)
direction "sw" = (-1, 0)
direction "ne" = (1, 0)
direction "s" = (0, -1)
direction "se" = (1, -1)

dist :: (Int, Int) -> Int
dist (a, b) = maximum [abs a, abs b, abs (a + b)]

advent11_1 :: String -> Int
advent11_1 a = dist $ walk $ parse a
