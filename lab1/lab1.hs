b = 12

fac 0 = 1
fac n = n * fac(n-1)

fac2 n = if n == 0 then 1 else n * fac2 (n - 1)

-- appendx []  x = [x]
-- appendx (h:l) x = h : appendx l x
appendx l x = foldr (:) [x] l

rev [] = []
rev (h:l) = appendx (rev l) h


infinity = infinity

-- infinity 5 pętli się

f x = 7

-- f (infinity 42) nie pętli się

rev2 = foldl (flip (:)) []

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibHelper 0 a b = a
fibHelper n a b = fibHelper (n - 1) b (a + b)
fib2 n = fibHelper n 0 1

idAbc x = x

-- let a = e in f
-- (\ a -> f) (e)

infixl 6 +++
(+++) :: Int -> Int -> Int
x +++ y = (x * x) + (y * y)

nats n = n : nats (n + 1)
list = nats 0

initsHelper [] acc = acc
-- inits_helper (l:ls) (acc:accs) =
inits [] = [[]]
-- inits (l:ls) =

hd [] = error "Lista pusta nie ma głowy!"
hd (x:xs) = x

tl [] = error "Lista pusta nie ma ogona!"
tl (x:xs) = xs

mytake n l =
  if n == 0 then
    []
  else
    case l of
      [] -> error "Za krótka lista :c!"
      x:xs -> x : mytake (n-1) xs
