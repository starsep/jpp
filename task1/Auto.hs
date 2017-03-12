module Auto ( ) where
  data Auto a q = A { states :: [q], initStates  :: [q], isAccepting :: q -> Bool, transition  :: q -> a -> [q]}
  -- accepts :: Eq q => Auto a q -> [a] -> Bool
  emptyA :: Auto a ()
  emptyA = A { states = [], initStates = [], isAccepting = const False, transition = \ _ _ -> []}
  epsA :: Auto a ()
  epsA = A { states = [], initStates = [], isAccepting = const True, transition = \ _ _ -> []}
  symA :: Eq a => a -> Auto a Bool
  symA x = A { states = [False, True], initStates = [False], isAccepting = id, transition = t} where
    t q a = [True | not q && a == x]
  -- leftA :: Auto a q -> Auto a (Either q r)
  sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
  sumA aut1 aut2 = A { states = s, initStates = i, isAccepting = acc, transition = t} where
    s = map Left (states aut1) ++ map Right (states aut2)
    i = map Left (initStates aut1) ++ map Right (initStates aut2)
    acc q = case q of Left q1 -> isAccepting aut1 q1
                      Right q2 -> isAccepting aut2 q2
    t q a = case q of Left q1 -> map Left (transition aut1 q1 a)
                      Right q2 -> map Right (transition aut2 q2 a)
  -- thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
  -- fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
  -- toLists :: (Enum a,Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
