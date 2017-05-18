
import Auto
main = runSimpleTests
runSimpleTests = runTests tests where
     tests =
         [ ("null",s $ accepts emptyA "",s False)
         , ("eps1",s $ accepts epsA "",s True)
         , ("eps2",s $ accepts epsA "x",s False)
         , ("chr1",s $ accepts (symA 'x') "x",s True)
         , ("chr2",s $ accepts (symA 'x') "y",s False)
         , ("cliq",s $ accepts cliq (take 100 (repeat ())), s False)
         , ("fork", s $ accepts r128 a127b, s False) 
         ]
     s x = show x -- cannot eta (MR?)
     writeln = putStrLn
     runTests = mapM_ runTest
     runTest (n, a, b) =
        writeln(if a == b
                then n++" PASS"
                else n++" FAIL. Expected: " ++ b ++ " Actual: " ++ a)

cliq :: Auto () Int
cliq = fromLists qs init accList transList where
    qs = [1,2,3]
    init = qs
    accList = []
    transList = [(q,(),qs) | q <- qs]

infixl 6 >>>
(>>>) = thenA

infixl 5 |||
(|||) = sumA

data AB = A | B deriving (Eq,Enum,Bounded,Show)
        
branchA = symA A `sumA` symA A
r1 = branchA
r2 = r1 >>> r1
r4 = r2 >>> r2
r8 = r4 >>> r4
r16 = r8 >>> r8
r20 = r16 >>> r4
r24 = r16 >>> r8
r28 = r24 >>> r4
r32 = r16 >>> r16
r64 = r32 >>> r32      
r128 = r64 >>> r64
r127 = r64 >>> r32 >>> r16 >>> r8 >>> r4 >>> r2 >>> r1
r127b = r127 >>> symA B
a = repeat A
a127 = take 127 a
a128 = take 128 a
a127b = a127 ++ [B]
