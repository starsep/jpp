import Data.Char
readInts :: String -> [Int]
readInts input = map (foldl (\acc x -> 10 * acc + (digitToInt x - digitToInt '0')) 0) $ filter (all isDigit) $ words input
readInts2 :: String -> Either String [Int]
readInts2 input = mapToInts $ checkError $ words input where
  mapToInts q =
    case q of Left err -> Left err
              Right x -> Right $ map (foldl (\acc x -> 10 * acc + (digitToInt x - digitToInt '0')) 0) x
  checkError q = if all (all isDigit) q then Right q else Left ":<<<"
