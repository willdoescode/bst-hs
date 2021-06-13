import Bst
import qualified Test.HUnit as T

testCaseAnswer, testInvertedAnswer, testRemoveAnswer :: Num a => Node a
testCaseAnswer = Node {value = 5, right = Just (Node {value = 6, right = Just (Node {value = 7, right = Nothing, left = Nothing}), left = Nothing}), left = Just (Node {value = 3, right = Nothing, left = Nothing})}
testInvertedAnswer = Node {value = 5, right = Just (Node {value = 3, right = Nothing, left = Nothing}), left = Just (Node {value = 6, right = Nothing, left = Just (Node {value = 7, right = Nothing, left = Nothing})})}
testRemoveAnswer = Node {value = 5, right = Just (Node {value = 6, right = Nothing, left = Nothing}), left = Just (Node {value = 3, right = Nothing, left = Nothing})}

testInsert :: T.Test
testInsert =
  T.TestCase $
    T.assertEqual
      "BST Should insert correctly"
      testCaseAnswer
      (insert (insert (insert (newList 5) 6) 7) 3)

testInsertList :: T.Test
testInsertList =
  T.TestCase $
    T.assertEqual
      "BST Should insert list correctly"
      testCaseAnswer
      ( insertList
          (newList 5)
          [6, 7, 3]
      )

testInvertTree :: T.Test
testInvertTree =
  T.TestCase $
    T.assertEqual
      "BST Should invert"
      testInvertedAnswer
      ( invert $
          insertList
            (newList 5)
            [6, 7, 3]
      )

testHeight :: T.Test
testHeight =
  T.TestCase $
    T.assertEqual
      "Height should be correct"
      3
      (height $ insertList (newList 5) [6, 7, 3])

testRemove :: T.Test
testRemove =
  T.TestCase $
    T.assertEqual
      "Removal from list should work"
      testRemoveAnswer
      (remove (insertList (newList 5) [6, 7, 8, 9, 3]) 7)

tests :: T.Test
tests = T.TestList [testInsert, testInsertList, testInvertTree, testHeight, testRemove]

main :: IO T.Counts
main = T.runTestTT tests
