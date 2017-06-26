swap (x, y) = (y, x)

class Yarr a where
    arr   :: (b -> c) -> a b c
    (>>>) :: a b c -> a c d -> a b d
    f1    :: a b c -> a (b, d) (c, d)
    f2    :: a b c -> a (d, b) (d, c)
    f2 f = arr swap >>> f1 f >>> arr swap

instance Yarr (->) where
    arr = id
    (>>>) = flip (.)
    f1 f (b, d) = (f b, d)

r1 f g = f1 f >>> f2 g
r2 f g = f2 g >>> f1 f

main = do
    print $ r1 (+1) (*2) (3, 4)
    print $ r2 (+1) (*2) (3, 4)
