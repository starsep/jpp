b = 12

fac 0 = 1
fac n = n * fac(n-1)

fac2 n = if n == 0 then 1 else n * fac2 (n - 1)

appendx []  x = [x]
appendx (h:l) x = h:(appendx l x)

rev [] = []
rev (h:l) = appendx (rev l) h


infinity x = infinity x

-- infinity 5 pętli się

f x = 7

-- f (infinity 42) nie pętli się

rev_pom [] l = l
rev_pom (x:xs) l = rev_pom xs (x:l)
rev2 l = rev_pom l []

fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fib_helper 0 a b = a
fib_helper n a b = fib_helper (n - 1) b (a + b)
fib2 n = fib_helper n 0 1

id_abc = \ x -> x

-- let a = e in f
-- (\ a -> f) (e)

infixl 6 +++
(+++) :: Int -> Int -> Int
x +++ y = (x * x) + (y * y)

nats n = n : (nats (n + 1))
list = nats 0

inits_helper [] acc = acc
inits_helper (l:ls) (acc:accs) =  
inits [] = [[]]
inits (l:ls) =
