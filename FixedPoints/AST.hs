{-# LANGUAGE DeriveFunctor #-}

import Data.Functor.Foldable 

data Op = Add | Mult deriving Show
data ASTF r = BinOpF Op r r | Num Int 

-- check if this is right
instance Functor ASTF where
    fmap f (BinOpF op e d) = BinOpF op (f e) (f d)
    fmap _ (Num a) = (Num a)  

simpleExprFix :: Fix ASTF
simpleExprFix = Fix (BinOpF Mult (Fix (BinOpF Add (Fix (Num 1)) (Fix (Num 2)))) (Fix (Num 3)))


interpret :: Fix ASTF -> Int
interpret = cata algebra where
    algebra :: ASTF Int -> Int
    algebra (Num n) = n
    algebra (BinOpF Add a b) = a + b
    algebra (BinOpF Mult a b) = a * b
