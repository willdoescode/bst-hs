module Bst where

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

height' :: Maybe (Node a) -> Int
height' Nothing = 0
height' (Just Node {value = _, right = r, left = l}) = 1 + max (height' r) (height' l)

height :: Node a -> Int
height = height' . pure
