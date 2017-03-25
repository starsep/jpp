module Auto (Auto, accepts, emptyA, epsA, symA, leftA, sumA, thenA, fromLists, toLists) where
  import Data.List

  data Auto a q = A { states :: [q], initStates :: [q], isAccepting :: q -> Bool, transition :: q -> a -> [q] }

  getNewStates :: Eq q => Auto a q -> a -> [q] -> [q]
  getNewStates aut x qs = nub $ foldl (\ acc state -> acc ++ transition aut state x) [] qs

  acceptsHelper :: Eq q => Auto a q -> [a] -> [q] -> Bool
  acceptsHelper aut [] qs = any (isAccepting aut) qs
  acceptsHelper aut (x:xs) qs = acceptsHelper aut xs (getNewStates aut x qs)

  accepts :: Eq q => Auto a q -> [a] -> Bool
  accepts aut word = acceptsHelper aut word (initStates aut)

  emptyA :: Auto a ()
  emptyA = A { states = [], initStates = [], isAccepting = const False, transition = \ _ _ -> []}

  epsA :: Auto a ()
  epsA = A { states = [()], initStates = [()], isAccepting = const True, transition = \ _ _ -> []}

  symA :: Eq a => a -> Auto a Bool
  symA x = A { states = [False, True], initStates = [False], isAccepting = id, transition = t} where
    t q a = [True | not q && a == x]

  leftA :: Auto a q -> Auto a (Either q r)
  leftA aut = A { states = map Left (states aut), initStates = map Left (initStates aut), isAccepting = isA, transition = trans} where
    isA (Left x) = isAccepting aut x
    isA (Right _) = False
    trans (Left x) c = map Left (transition aut x c)
    trans (Right _) _ = []

  sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
  sumA aut1 aut2 = A { states = s, initStates = i, isAccepting = acc, transition = t} where
    s = map Left (states aut1) ++ map Right (states aut2)
    i = map Left (initStates aut1) ++ map Right (initStates aut2)
    acc q = case q of Left q1 -> isAccepting aut1 q1
                      Right q2 -> isAccepting aut2 q2
    t q a = case q of Left q1 -> map Left (transition aut1 q1 a)
                      Right q2 -> map Right (transition aut2 q2 a)

  thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
  thenA aut1 aut2 = A { states = s, initStates = i, isAccepting = acc, transition = t} where
    s = map Left (states aut1) ++ map Right (states aut2)
    i = map Left (initStates aut1)
    acc q = case q of Left q1 -> isAccepting aut1 q1
                      Right q2 -> isAccepting aut2 q2
    t q a = case q of Left q1 -> map Left (transition aut1 q1 a) ++ if isAccepting aut1 q1 then map Right (initStates aut2) else []
                      Right q2 -> map Right (transition aut2 q2 a)

  fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q, a, [q])] -> Auto a q
  fromLists s i a t = A { states = s, initStates = i, isAccepting = (`elem` a), transition = trans} where
    trans q c = foldl (\ acc (x, cc, l) -> if x == q && c == cc then l ++ acc else acc) [] t

  toLists :: (Enum a, Bounded a) => Auto a q -> ([q], [q], [q], [(q, a, [q])])
  toLists aut = (states aut, initStates aut, filter (isAccepting aut) (states aut), trans) where
    valuesA :: (Enum a, Bounded a) => [a]
    valuesA = [minBound .. maxBound]
    transitionsA x c = x ++ foldl (\acc s -> let t = transition aut s c in if null t then acc else (s, c, t) : acc) [] (states aut)
    trans = foldl transitionsA [] valuesA

  instance (Show a, Enum a, Bounded a, Show q) => Show (Auto a q) where
    show aut = show s ++ " " ++ show i ++ " " ++ show acc ++ " " ++ show t where
      (s, i, acc, t) = toLists aut
