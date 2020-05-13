module MonadPlus where

import Alternative
import Data.Monoid

class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus Maybe where
    mzero = empty
    mplus = (<|>)

msum :: (Traversable t, MonadPlus m) => t (m a) -> m a
msum = foldr mplus mzero

mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
mfilter p x = do
    x' <- x
    if p x' then return x' else mzero
