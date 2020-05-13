module Reader where

import Profunctor

newtype Reader r a =
    Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ \r -> f . ra $ r

instance Applicative (Reader r) where
    pure = Reader . const
    (Reader ab) <*> (Reader a) = Reader $ \r -> ab r (a r)

instance Monad (Reader r) where
    return = pure
    (Reader f) >>= amb = Reader $ \r -> runReader (amb (f r)) r

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = f <$> ask

withReader :: (r -> s) -> Reader s a -> Reader r a
withReader f (Reader sa) = Reader $ sa . f

local :: (r -> r) -> Reader r a -> Reader r a
local = withReader

instance Profunctor Reader where
    lmap f (Reader r) = Reader $ r . f
    rmap g (Reader r) = Reader $ g . r
    dimap f g (Reader r) = Reader $ g . r . f
