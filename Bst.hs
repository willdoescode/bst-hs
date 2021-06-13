module Bst
  ( Node (..),
    newTree,
    insert,
    invert,
    insertList,
    height,
    remove,
  )
where

import Data.Maybe (fromJust)

data Node a = Node
  { value :: a,
    right :: Maybe (Node a),
    left :: Maybe (Node a)
  }
  deriving (Show, Eq)

newTree :: a -> Node a
newTree x =
  Node
    { value = x,
      right = Nothing,
      left = Nothing
    }

insert' :: Ord a => Maybe (Node a) -> a -> Node a
insert' Nothing x = newTree x
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
invert' (Just head) =
  Just $
    Node
      { value = value head,
        right = invert' (left head),
        left = invert' (right head)
      }

invert :: Node a -> Node a
invert = fromJust . invert' . pure

height' :: Maybe (Node a) -> Int
height' Nothing = 0
height' (Just Node {right = r, left = l}) = 1 + max (height' r) (height' l)

height :: Node a -> Int
height = height' . pure

remove' :: Eq a => Maybe (Node a) -> a -> Maybe (Node a)
remove' Nothing _ = Nothing
remove' (Just Node {value = val, right = r, left = l}) v
  | val == v = Nothing
  | otherwise =
    Just $
      Node
        { value = val,
          right = remove' r v,
          left = remove' l v
        }

remove :: Eq a => Node a -> a -> Node a
remove head v = fromJust $ remove' (pure head) v
