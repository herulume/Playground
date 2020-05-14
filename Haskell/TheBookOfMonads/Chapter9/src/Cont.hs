{-# LANGUAGE RankNTypes #-}
module Cont where

newtype Cont r a =
    Cont
    { runCont :: (a -> r) -> r }

toCont :: a -> (forall r. Cont r a)
toCont a = Cont $ \f -> f a

fromCont :: (forall r. Cont r a) -> a
fromCont (Cont f) = f id

instance Functor (Cont r) where
    fmap f (Cont c) = Cont $ \bToR -> c (bToR . f)

instance Applicative (Cont r) where
    pure x = Cont $ \c -> c x
    (Cont f) <*> (Cont v) = Cont $ \bToR ->
        f $ \aToB -> v (bToR . aToB)

instance Monad (Cont r) where
    return = pure
    (Cont m) >>= f = Cont $ \bToR ->
        m $ \a -> runCont (f a) bToR
