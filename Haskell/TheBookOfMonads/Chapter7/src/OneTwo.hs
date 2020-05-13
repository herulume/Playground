module OneTwo where

data OneTwo e r = L e | R r

instance Functor (OneTwo e) where
    fmap _ (L e) = L e
    fmap f (R e) = R $ f e

instance Applicative (OneTwo e) where
    pure = R
    (R f) <*> (R x) = R $ f x
    (L e) <*> _ = L e
    (R _) <*> (L e) = L e

instance Monad (OneTwo e) where
    return = pure
    R x >>= f = f x
    L e >>= _ = L e

