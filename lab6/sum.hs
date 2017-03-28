import Control.Monad.State

data Tree a = Node (Tree a) a (Tree a) | Leaf

sumTree :: Tree Int -> (Int -> ((), Int))
