add :: [Int] -> [Int] -> [Int]
add l [] = l
add [] l = l
add (x:xs) (y:ys) = (x + y):(add xs ys)

fib :: [Int]
fib = 1:(add fib (0:fib))

indexOf :: Char -> String -> Maybe Int
indexOf_helper c "" _ = Nothing
indexOf_helper c (x:xs) n =
  if c == x then
    Just n
  else
    indexOf_helper c xs (n + 1)
indexOf c s = indexOf_helper c s 0
