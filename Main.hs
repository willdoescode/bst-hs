module Main where

import Bst

main :: IO ()
main = print $ invert $ insertList (newTree 5) [6, 7, 3] -- == insert (insert (insert (newList 5) 6) 7) 3
