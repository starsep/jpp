module Auto ( ) where
  data Auto a q = A { states :: [q], initStates  :: [q], isAccepting :: q -> Bool, transition  :: q -> a -> [q]}
  accepts :: Eq q => Auto a q -> [a] -> Bool
  accepts _ _ = True
  emptyA :: Auto a ()
  emptyA _ = ()
  epsA :: Auto a ()
  epsA _ = ()
  symA :: Eq a => a -> Auto a Bool
  leftA :: Auto a q -> Auto a (Either q r)
  sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
  thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
  fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
  toLists :: (Enum a,Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
