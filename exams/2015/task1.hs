a :: (t -> Bool) -> [t] -> Bool
a p (x:xs) = p x && a p xs

z :: (a -> b -> c) -> [a] -> [b] -> [c]
z f (x:xs) (y:ys) = f x y : z f xs ys

c :: [Int]
c = 0 : 1 : z (+) c (tail c)

main = do
    print $ drop 9 . take 10 $ c
    print $ take 1 $ drop 9 c
    print $ a (<10) c
