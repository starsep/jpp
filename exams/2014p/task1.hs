class Zap f where
    fizz :: a -> f a
    zap :: f (a -> b) -> f a -> f b
instance Zap [] where
    fizz a = a : fizz a
    zap (f:fs) (x:xs) = f x : zap fs xs
zeq :: [[a]] -> [[a]]
zeq [] = fizz []
zeq (x:xs) = (fizz (:) `zap` x) `zap` zeq xs
main = do
    print $ zeq ([[1, 2, 3], [4, 5, 6]] :: [[Int]])
    -- print $ zeq ([[]] :: [[Int]])
    -- print $ head $ zeq ([] :: [[Int]])
