import System.Environment

main :: IO ()
main = do {
    args <- getArgs;
    putStr (unlines args)
}
