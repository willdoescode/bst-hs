module Spec where

import Bst
import qualified Test.HUnit as T

testCaseAnswer, invertedTestCaseAnswer :: Num a => Node a
testCaseAnswer = Node {value = 5, right = Just (Node {value = 6, right = Just (Node {value = 7, right = Nothing, left = Nothing}), left = Nothing}), left = Just (Node {value = 3, right = Nothing, left = Nothing})}
invertedTestCaseAnswer = Node {value = 5, right = Just (Node {value = 3, right = Nothing, left = Nothing}), left = Just (Node {value = 6, right = Nothing, left = Just (Node {value = 7, right = Nothing, left = Nothing})})}

testInsert :: T.Test
testInsert =
  T.TestCase $
    T.assertEqual
      "BST Should insert correctly"
      (insert (insert (insert (newList 5) 6) 7) 3)
      testCaseAnswer

testInsertList :: T.Test
testInsertList =
  T.TestCase $
    T.assertEqual
      "BST Should insert list correctly"
      ( insertList
          (newList 5)
          [6, 7, 3]
      )
      testCaseAnswer

testInvertlist :: T.Test
testInvertlist =
  T.TestCase $
    T.assertEqual
      "BST Should invert"
      ( invert $
          insertList
            (newList 5)
            [6, 7, 3]
      )
      invertedTestCaseAnswer

testHeight :: T.Test
testHeight =
  T.TestCase $
    T.assertEqual
      "Height should be correct"
      (height $ insertList (newList 5) [6, 7, 3])
      3
