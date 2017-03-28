import Control.Monad.Reader

data Help a = That { fun :: Int -> a }

-- instance Monad Help where
  -- return a = That (const a)
  -- (>>=) (That f) update = That $ \ x -> (fun $ update $ f x) x

-- ask :: Help Int
-- ask = That id

-- local :: (Int -> Int) -> Help a -> Help a
-- local upd (That f) = That $ f . upd

data Tree a = Node (Tree a) a (Tree a) | Leaf

treeDepth :: Tree a -> Tree (Int, a)
treeDepth t = treeDRec t 0

treeDRec :: Tree a -> Int -> Tree (Int, a)
-- treeDRec :: Tree a -> Help (Tree (Int, a))
treeDRec Leaf = return Leaf
treeDRec (Node tl x tr) = do {
  d <- ask;
  ntl <- local (+1) (treeDRec tl);
  ntr <- local (+1) (treeDRec tr);
  return (Node ntl (d, x) ntr)
}
