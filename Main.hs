module Main where

import qualified Test.HUnit as T

data Node a = Node
  { value :: a,
    right :: Maybe (Node a),
    left :: Maybe (Node a)
  }
  deriving (Show, Eq)

newList :: a -> Node a
newList x = Node {value = x, right = Nothing, left = Nothing}

insert' :: Ord a => Maybe (Node a) -> a -> Node a
insert' Nothing x = newList x
insert' (Just head) val
  | value head > val =
    Node
      { value = value head,
        right = right head,
        left = Just $ insert' (left head) val
      }
  | otherwise =
    Node
      { value = value head,
        right = Just $ insert' (right head) val,
        left = left head
      }

insert :: Ord a => Node a -> a -> Node a
insert head = insert' (Just head)

insertList :: Ord a => Node a -> [a] -> Node a
insertList = foldl insert

invert' :: Maybe (Node a) -> Maybe (Node a)
invert' Nothing = Nothing
invert' (Just head) = Just $ Node {value = value head, right = invert' (left head), left = invert' (right head)}

unwrap :: Maybe (Node a) -> Node a
unwrap (Just head) = head
unwrap Nothing = error "handle error"

invert :: Node a -> Node a
invert = unwrap . invert' . pure

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

main :: IO T.Counts
main = T.runTestTT $ T.TestList [testInsert, testInsertList, testInvertlist]

-- main :: IO ()
-- main = print $ invert $ insertList (newList 5) [6, 7, 3] -- == insert (insert (insert (newList 5) 6) 7) 3
