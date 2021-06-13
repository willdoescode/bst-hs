module Main where

import Spec
import qualified Test.HUnit as T

main :: IO T.Counts
main = T.runTestTT $ T.TestList [testInsert, testInsertList, testInvertlist, testHeight]

-- main :: IO ()
-- main = print $ invert $ insertList (newList 5) [6, 7, 3] -- == insert (insert (insert (newList 5) 6) 7) 3
