import System.Environment
import Data.Maybe
import Text.Read

-- assert (warunek) $ x jest równoważne
-- if warunek then x else Nothing
assert :: Bool -> Maybe a -> Maybe a
assert False _ = Nothing
assert True x = x

assertJust :: Maybe t -> Maybe a -> Maybe a
assertJust = assert . isJust

parseStateList :: (String, Int) -> Maybe [Int]
parseStateList (line, n) =
  let maybeStateList = readMaybe line :: Maybe [Int] in
  assertJust maybeStateList $
  let stateList = fromJust maybeStateList :: [Int] in
  assert (all (\x -> x > 0 && x <= n) stateList) $
  maybeStateList

runAuto :: String -> Maybe Bool
runAuto file =
  let fileLines = filter (not . null) . lines $ file :: [String] in
  assert (length fileLines >= 4) $
  let maybeStatesNumber = readMaybe $ head fileLines :: Maybe Int in
  assertJust maybeStatesNumber $
  let n = fromJust maybeStatesNumber :: Int
      maybeStartingStates = parseStateList (fileLines !! 1, n) :: Maybe [Int]
      maybeAcceptingStates = parseStateList (fileLines !! 2, n) :: Maybe [Int] in
  assertJust maybeStartingStates $
  assertJust maybeAcceptingStates $
  let accStates = fromJust maybeAcceptingStates
      initStates = fromJust maybeStartingStates in
  Just (n > 2)

errorString :: String
errorString = "BAD INPUT"

resultToString :: Maybe Bool -> String
resultToString Nothing = errorString
resultToString (Just x) = show x

main :: IO ()
main = do {
    args <- getArgs;
    if null args then
      putStrLn "Brakuje parametru!"
    else do {
      -- Zakładam, że istnieje plik o nazwie podanej w argumencie
      file <- readFile (head args);
      putStrLn . resultToString . runAuto $ file
    }
}
