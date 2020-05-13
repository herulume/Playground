module Predicate where

import Contravariant

newtype Predicate a =
    Predicate { runPredicate :: a -> Bool }

through :: (a -> b) -> Predicate b -> Predicate a
through f (Predicate p) = Predicate (p . f)

instance Contravariant Predicate where
    contramap = through
