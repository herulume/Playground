{-# LANGUAGE RankNTypes #-}


data Unit = Unit -- Guess what, it's a fucking unit m8
data Prod a b = P a b -- cross product
data Sum a b = L a | R b  -- disjoint union
newtype Mu f = Mu (forall a . (f a -> a) -> a) -- least fixed point

-- Nats
zeroMu = Mu $ \f -> f $ L Unit
succMu (Mu g) = Mu $ \f -> f $ R $ g f




