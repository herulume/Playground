newtype Fix f = Fix { unFix :: f (Fix f) }

data NatF r =  Zero | SuccF r deriving Show
type Nat   =  Fix NatF

zero :: Nat
zero = Fix Zero

succ :: Nat -> Nat
succ = Fix . SuccF

instance Functor NatF where
    fmap _ Zero = Zero
    fmap f (SuccF e) = SuccF $ f e

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . (fmap (cata f)) . unFix

eval :: Nat ->  Int 
eval = cata algebra

algebra :: NatF Int -> Int
algebra Zero = 0
algebra (SuccF r) = r + 1


example = Fix (SuccF (Fix (SuccF (Fix Zero))))