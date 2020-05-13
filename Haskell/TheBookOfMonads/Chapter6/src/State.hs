{-# LANGUAGE TupleSections #-}
module State where

newtype State s a =
    State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State sf) =  State $ \s -> let (a, s') = sf s in (f a, s')

instance Applicative (State s) where
    pure x = State (x,)
    (State fs) <*> (State as) = State $ \s -> let (f, s') = fs s
                                                  (a, s'') = as s'
                                               in (f a, s'')

instance Monad (State s) where
    return = pure
    (State ma) >>= amb = State $ \s -> let (a, s') = ma s
                                         in runState (amb a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify as = get >>= put . as

evalState :: State s a -> s -> a
evalState fs = fst . runState fs

execState :: State s a -> s -> s
execState fs = snd . runState fs

nextValue :: State Int Int
nextValue = get >>= \i -> put (i+1) >> pure i
