module RWS where

import Data.Monoid

newtype RWS r w s a =
    RWS { runRWS :: r -> s -> (a, s, w) }

instance Functor (RWS r w s) where
    fmap f (RWS rws) = RWS $ \r s -> let (a, s, w) = rws r s in (f a, s, w)

instance Monoid w => Applicative (RWS r w s) where
    pure a = RWS $ \r s -> (a, s, mempty)
    (RWS f) <*> (RWS a) = RWS $ \r s -> let (ab, s', w') = f r s
                                            (a', s'', w'') = a r s'
                                         in (ab a', s'', w' <> w'')

instance Monoid w => Monad (RWS r w s) where
    return = pure
    (RWS ma) >>= amb = RWS $ \r s -> let (a', s', w') = ma r s
                                         (b, s'', w'') = runRWS (amb a') r s'
                                      in (b, s'', w' <> w'')

reader :: Monoid w => (r -> a) -> RWS r w s a
reader f = RWS $ \r s -> (f r, s, mempty)

writer :: (a, w) -> RWS r w s a
writer (a, w) = RWS $ \_ s -> (a, s, w)

state :: Monoid w => (s -> (a, s)) -> RWS r w s a
state f = RWS $ \r s -> let (a, s') = f s in (a, s', mempty)
