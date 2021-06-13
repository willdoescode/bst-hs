import Bst
import Test.HUnit hiding (Node)

testCaseAnswer, testInvertedAnswer, testRemoveAnswer :: Num a => Node a
testCaseAnswer = Node {value = 5, right = Just (Node {value = 6, right = Just (Node {value = 7, right = Nothing, left = Nothing}), left = Nothing}), left = Just (Node {value = 3, right = Nothing, left = Nothing})}
testInvertedAnswer = Node {value = 5, right = Just (Node {value = 3, right = Nothing, left = Nothing}), left = Just (Node {value = 6, right = Nothing, left = Just (Node {value = 7, right = Nothing, left = Nothing})})}
testRemoveAnswer = Node {value = 5, right = Just (Node {value = 6, right = Nothing, left = Nothing}), left = Just (Node {value = 3, right = Nothing, left = Nothing})}

testInsert :: Test
testInsert =
  TestCase $
    assertEqual
      "BST Should insert correctly"
      testCaseAnswer
      (insert (insert (insert (newTree 5) 6) 7) 3)

testInsertList :: Test
testInsertList =
  TestCase $
    assertEqual
      "BST Should insert list correctly"
      testCaseAnswer
      ( insertList
          (newTree 5)
          [6, 7, 3]
      )

testInvertTree :: Test
testInvertTree =
  TestCase $
    assertEqual
      "BST Should invert"
      testInvertedAnswer
      ( invert $
          insertList
            (newTree 5)
            [6, 7, 3]
      )

testHeight :: Test
testHeight =
  TestCase $
    assertEqual
      "Height should be correct"
      3
      (height $ insertList (newTree 5) [6, 7, 3])

testRemove :: Test
testRemove =
  TestCase $
    assertEqual
      "Removal from list should work"
      testRemoveAnswer
      (remove (insertList (newTree 5) [6, 7, 8, 9, 3]) 7)

tests :: Test
tests = TestList [testInsert, testInsertList, testInvertTree, testHeight, testRemove]

main :: IO Counts
main = runTestTT tests
