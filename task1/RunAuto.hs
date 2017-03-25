import           Auto
import           Data.Char
import           Data.Maybe
import           System.Environment
import           Text.Read

-- assert (warunek) $ x jest równoważne
-- if warunek then x else Nothing
assert :: Bool -> Maybe a -> Maybe a
assert False _ = Nothing
assert True x  = x

assertJust :: Maybe t -> Maybe a -> Maybe a
assertJust = assert . isJust

parseStateList :: (String, Int) -> Maybe [Int]
parseStateList (line, n) =
  let maybeStateList = readMaybe line :: Maybe [Int] in
  assertJust maybeStateList $
  let stateList = fromJust maybeStateList :: [Int] in
  assert (all (\x -> x > 0 && x <= n) stateList)
  maybeStateList

parseWord :: String -> Maybe String
parseWord word = assert (all isAsciiUpper word) $ Just word

parseStatesHelper :: [String] -> [Int] -> Maybe [Int]
parseStatesHelper [] acc = Just acc
parseStatesHelper (s : ss) acc =
  let state = readMaybe s :: Maybe Int in
  assertJust state $
  parseStatesHelper ss (fromJust state : acc)

parseStates :: [String] -> Maybe [Int]
parseStates ls = parseStatesHelper ls []

parseTransition :: String -> Maybe [(Int, Char, [Int])]
parseTransition line =
  let splited = words line :: [String] in
  assert (length splited >= 3) $
  let maybeInit = readMaybe (head splited) :: Maybe Int
      maybeSymbols = parseWord (splited !! 1) :: Maybe String
      maybeStates = parseStates (drop 2 splited) :: Maybe [Int] in
  assertJust maybeInit $
  assertJust maybeSymbols $
  assertJust maybeStates $
  let i = fromJust maybeInit :: Int
      symbols = fromJust maybeSymbols :: String
      states = fromJust maybeStates :: [Int] in
  Just $ map (\s -> (i, s, states)) symbols

parseTransitionsHelper :: [String] -> [(Int, Char, [Int])] -> Maybe ([(Int, Char, [Int])], String)
parseTransitionsHelper [] _ = Nothing
parseTransitionsHelper [word] acc =
  assertJust (parseWord word) $
  Just (acc, word)
parseTransitionsHelper (t : ts) acc =
  let maybeTransition = parseTransition t :: Maybe [(Int, Char, [Int])] in
  assertJust maybeTransition $
  parseTransitionsHelper ts (fromJust maybeTransition ++ acc)

parseTransitions :: [String] -> Maybe ([(Int, Char, [Int])], String)
parseTransitions fileLines = parseTransitionsHelper fileLines []

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
  let accStates = fromJust maybeAcceptingStates :: [Int]
      initStates = fromJust maybeStartingStates :: [Int]
      maybeParseTransitions = parseTransitions (drop 3 fileLines) in
  assertJust maybeParseTransitions $
  -- zakładam, że stany na wejściu są w [1..n]
  let (trans, word) = fromJust maybeParseTransitions
      auto = fromLists [1..n] initStates accStates trans in
  Just $ accepts auto word

errorString :: String
errorString = "BAD INPUT"

resultToString :: Maybe Bool -> String
resultToString Nothing  = errorString
resultToString (Just x) = show x

main :: IO ()
main = do {
    args <- getArgs;
    if null args then
      putStrLn "Brakuje parametru!"
    else do {
      -- Zakładam, że istnieje plik o nazwie podanej w argumencie
      -- w p.p. wyjątek
      file <- readFile (head args);
      putStrLn . resultToString . runAuto $ file
    }
}
