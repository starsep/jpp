data Tree a = Leaf | Node (Tree a) a (Tree a)

sumT :: Tree Integer -> Integer
sumT Leaf = 0
sumT (Node t1 x t2) = x + sumT t1 + sumT t2

xx = Node (Node Leaf 1 Leaf) 5 Leaf

rotT :: Tree a -> Tree a
rotT Leaf = Leaf
rotT (Node t1 x t2) = Node (rotT t2) x (rotT t1)

instance Show(a) => Show(Tree a) where
  show Leaf = "Leaf"
  show (Node t1 x t2) =
    "{" ++ st1 ++ " " ++ sx ++ " " ++ st2 ++ "}" where
      st1 = show t1
      sx = show x
      st2 = show t2

-----

revL :: [a] -> [a]
revL = foldl (flip (:)) []

----

existsT :: (Ord a) => Tree a -> a -> Bool
existsT Leaf _ = False
existsT (Node t1 x t2) v = (x == v) || existsT t1 v || existsT t2 v

existsBST :: (Ord a) => Tree a -> a -> Bool
existsBST Leaf _ = False
existsBST (Node t1 x t2) v =
  (x == v) ||
  if v < x then
    existsBST t1 v
  else
    existsBST t2 v

insertT :: (Ord a) => Tree a -> a -> Tree a
-- insert t x
