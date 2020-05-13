module Alternative where

import Data.Monoid

class Applicative m => Alternative m where
    empty :: m a
    (<|>) :: m a -> m a -> m a

guard :: Alternative m => Bool -> m ()
guard True = pure ()
guard False = empty

instance Alternative Maybe where
    empty = Nothing
    Just x <|> _ = Just x
    Nothing <|> o = o

instance Alternative [] where
    empty = mempty
    (<|>) = (<>)

asum :: (Traversable t, Alternative m) => t (m a) -> m a
asum = foldr (<|>) empty

