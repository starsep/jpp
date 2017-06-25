f :: (Num a, Show a) => a -> (a, IO ())
f n = (3 * n + 1, print n)

g :: (Integral a, Show a) => a -> IO ()
g n = let (m, a) = f n in if even m then a else return ()

main :: IO ()
main = mapM_ g [1..10]
