exists p (x:xs) = p x || exists p xs
f (a:x) y = a:(f y x)
c = 0 : f (map (+1) c) (tail c)

main = do
    print $ take 10 c
    print $ exists (>10) c
    print $ exists (<0) c

