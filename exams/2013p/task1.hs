f :: Int -> [Int]
f n = do
    x <- [1..n]
    if even x then
        return ()
    else
        []
    return 3

main = do
    print $ f 1
    print $ f 10
