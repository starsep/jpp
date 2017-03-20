main :: IO ()
main = do {
    putStrLn "Ulubiony język programowania?";
    line <- getLine;
    if line /= "Haskell" then
      do {
        putStrLn "Źle!";
        main
      }
    else
      putStrLn "Super! Mój też!"
}
