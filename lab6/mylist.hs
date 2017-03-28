import Control.Monad.Reader
import Control.Applicative
import Control.Monad (liftM, ap)

data MyList a = That { getL :: [a] }

instance Monad MyList where
  return a = That [a]
  (>>=) (That l) update = That $ concatMap (getL . update l)

instance Functor MyList where
  fmap = liftM

instance Applicative MyList where
  pure  = return
  (<*>) = ap
