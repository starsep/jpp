-- Filip Czaplicki fc359081@students.mimuw.edu.pl

module Auto (Auto, accepts, emptyA, epsA, symA, leftA, sumA, thenA, fromLists, toLists) where
  import Data.List

  data Auto a q = A {
    states :: [q],
    initStates :: [q],
    isAccepting :: q -> Bool,
    transition :: q -> a -> [q]
  }

  -- Funkcja pomocnicza do accepts.
  -- Dostaje automat, literę oraz listę stanów.
  -- Zwraca nową listę stanów.
  getNewStates :: Eq q => Auto a q -> a -> [q] -> [q]
  getNewStates aut x qs =
    nub $ foldr (\ state acc -> transition aut state x ++ acc) [] qs

  -- Funkcja pomocnicza do sprawdzania czy automat akceptuje słowo puste.
  acceptsEmpty :: Auto a q -> Bool
  acceptsEmpty aut = any (isAccepting aut) (initStates aut)

  -- Funkcja pomocnicza do accepts.
  -- Dostaje automat, wyraz, listę stanów.
  -- Zwraca wartość logiczną taką jak accepts.
  acceptsHelper :: Eq q => Auto a q -> [a] -> [q] -> Bool
  acceptsHelper aut [] qs = any (isAccepting aut) qs
  acceptsHelper aut (x:xs) qs = acceptsHelper aut xs (getNewStates aut x qs)

  accepts :: Eq q => Auto a q -> [a] -> Bool
  accepts aut word = acceptsHelper aut word (initStates aut)

  emptyA :: Auto a ()
  emptyA = A {
    states = [],
    initStates = [],
    isAccepting = const False,
    transition = \ _ _ -> []
  }

  epsA :: Auto a ()
  epsA = A {
    states = [()],
    initStates = [()],
    isAccepting = const True,
    transition = \ _ _ -> []
  }

  symA :: Eq a => a -> Auto a Bool
  symA x = A {
    states = [False, True],
    initStates = [False],
    isAccepting = id,
    transition = \ q a -> [True | not q && a == x]
  }

  leftA :: Auto a q -> Auto a (Either q r)
  leftA aut = A {
    states = map Left (states aut),
    initStates = map Left (initStates aut),
    isAccepting = \ q -> case q of
      Left x -> isAccepting aut x
      Right _ -> False,
    transition = \ q c -> case q of
      Left x -> map Left (transition aut x c)
      Right _ -> []
  }

  sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
  sumA aut1 aut2 = A {
    states = map Left (states aut1) ++ map Right (states aut2),
    initStates = map Left (initStates aut1) ++ map Right (initStates aut2),
    isAccepting = \ q -> case q of
      Left q1 -> isAccepting aut1 q1
      Right q2 -> isAccepting aut2 q2,
    transition = \ q a -> case q of
      Left q1 -> map Left (transition aut1 q1 a)
      Right q2 -> map Right (transition aut2 q2 a)
  }

  thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
  thenA aut1 aut2 = A {
    states = map Left (states aut1) ++ map Right (states aut2),
    initStates = map Right (
        if acceptsEmpty aut1 then
          initStates aut2
        else []
      ) ++ map Left (initStates aut1),
    isAccepting = \ q -> case q of
      Left q' -> acceptsEmpty aut2 && isAccepting aut1 q'
      Right q' -> isAccepting aut2 q',
    transition = \ q a -> case q of
      Left q' ->
        (if any (isAccepting aut1) (transition aut1 q' a) then
          map Right (initStates aut2)
        else []
        ) ++ map Left (transition aut1 q' a)
      Right q' -> map Right (transition aut2 q' a)
  }

  fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q, a, [q])] -> Auto a q
  fromLists s i a t = A {
    states = s,
    initStates = i,
    isAccepting = (`elem` a),
    transition = \ q c ->
      foldr (\ (x, cc, l) acc ->
        if x == q && c == cc then l ++ acc else acc
      ) [] t
  }

  toLists :: (Enum a, Bounded a) => Auto a q -> ([q], [q], [q], [(q, a, [q])])
  toLists aut =
    (states aut, initStates aut, filter (isAccepting aut) (states aut), trans) where
      transitionsA c acc =
        foldr (\ s acc' ->
          let t = transition aut s c in
          if null t then acc' else (s, c, t) : acc'
        ) [] (states aut) ++ acc
      trans = foldr transitionsA [] [minBound .. maxBound]

  instance (Show a, Enum a, Bounded a, Show q) => Show (Auto a q) where
    show aut = show s ++ " " ++ show i ++ " " ++ show acc ++ " " ++ show t where
      (s, i, acc, t) = toLists aut
