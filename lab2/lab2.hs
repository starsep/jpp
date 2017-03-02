add :: [Int] -> [Int] -> [Int]
add l [] = l
add [] l = l
add (x:xs) (y:ys) = (x + y):(add xs ys)

fib :: [Int]
fib = 1 : add fib (0:fib)

indexOf :: Char -> String -> Maybe Int
indexOfHelper c "" _ = Nothing
indexOfHelper c (x:xs) n =
  if c == x then
    Just n
  else
    indexOfHelper c xs (n + 1)
indexOf c s = indexOfHelper c s 0

positions :: Char -> String -> [Int]
positionsHelper :: Char -> String -> Int -> [Int] -> [Int]
positionsHelper _ "" _ acc = acc
positions_helper c (x:xs) n acc =
  positions_helper c xs (n + 1) new_acc where
    new_acc =
      if c == x then
        n:acc
      else
        acc
positions c s = reverse (positions_helper c s 0 [])
