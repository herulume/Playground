module Returns where

import Contravariant

newtype Returns r a = R (a -> r)

instance Contravariant (Returns r) where
    contramap g (R f) = R $ f . g
