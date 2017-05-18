{-# LANGUAGE TupleSections #-}
import Test.QuickCheck
import Auto

data AB = A | B deriving(Eq,Ord,Show,Bounded,Enum)

leftA' :: Auto a q -> Auto a (Either q ()) 
leftA' = leftA

testLeftA :: AutoDescr -> [AB] -> Bool
testLeftA ad w =  accepts a w == accepts (leftA' a) w where a = fromAD ad

-- rightA' :: Auto a q -> Auto a (Either p q)                                        -- rightA' = rightA                      
-- testRightA :: AutoDescr -> [AB] -> Bool
-- testRightA ad w =  accepts a w == accepts (rightA a) w where a = fromAD ad

testSumA1 :: AutoDescr -> AutoDescr -> [AB] -> Property
testSumA1 ad1 ad2 w = accepts a1 w ==> 
                          accepts (a1 `sumA` a2) w where
                              [a1,a2] = map fromAD [ad1,ad2]

testSumA :: AutoDescr -> AutoDescr -> [AB] -> Bool
testSumA ad1 ad2 w = ((accepts a1 w) || (accepts a2 w)) == 
                         (accepts (a1 `sumA` a2) w) where
                              [a1,a2] = map fromAD [ad1,ad2]

                                        
testThenA :: AutoDescr -> AutoDescr -> [AB] -> [AB] -> Property
testThenA ad1 ad2 w1 w2 = accepts a1 w1 ==> accepts a2 w2 ==>
                          accepts (a1 `thenA` a2) (w1++w2) where
                              [a1,a2] = map fromAD [ad1,ad2]

write = putStr
writeln = putStrLn

main = do
  -- writeln "testLeftA"
  -- quickCheck testLeftA
  -- writeln "testRightA"
  -- quickCheck testRightA
  writeln "testSumA"
  quickCheckWith stdArgs testSumA
  writeln "testSumA1"
  quickCheckWith stdArgs { maxDiscardRatio = 100 } testSumA1
  writeln "testThenA"
  quickCheck  testThenA
------------------------------------------------------------
-- Hic sunt leones
------------------------------------------------------------

instance Arbitrary AB where
  arbitrary = oneof [return A, return B]

newtype AutoDescr = AD  {unAD :: ([Int],[Int],[Int],[(Int,AB,[Int])])}
    deriving (Show)

fromAD :: AutoDescr -> Auto AB Int
fromAD (AD (qs,x,y,z)) = fromLists qs x y z

genNumSet :: (Integral t, Integral a) => a -> Gen [t]
genNumSet n = concat `fmap` sequence [ elements [[],[fromIntegral k]] | k <- [0..n]]

instance Arbitrary AutoDescr where         
   arbitrary = sized $ \size -> do
      nst <- choose (0,size)
      let qs = [0..(fromIntegral nst)] :: [Int]
      init <- genNumSet nst
      acc <- genNumSet nst
      transList <- sequence [(q,a,) `fmap` genNumSet nst | q <- qs, a <- [A,B]]
      return $ AD (qs, init, acc, transList)

