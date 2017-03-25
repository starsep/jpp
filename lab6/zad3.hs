data ParseError = Err {location::Int, reason::String}
instance Error ParseError ...
type ParseMonad = Either ParseError
parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHex :: String -> ParseMonad Integer
toString :: Integer -> ParseMonad String

-- convert zamienia napis z liczba szesnastkowa
--   na napis z liczba dziesietna
convert :: String -> String
convert s = str where
 (Right str) = tryParse s `catchError` printError
 tryParse s = do {n <- parseHex s; toString n}
 printError e = ...
