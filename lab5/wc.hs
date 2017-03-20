import System.Environment

wc :: String -> String -> String
wc file name = wcl ++ " " ++ wcw ++ " " ++ wcc ++ " " ++ name where
  wcl = show (length . lines $ file)
  wcw = show (length . words $ file)
  wcc = show (length file)

main :: IO ()
main = do {
    args <- getArgs;
    if null args then
      putStrLn "Brakuje parametru!"
    else do {
    file <- readFile (head args);
    putStrLn $ wc file (head args)
  }
}
