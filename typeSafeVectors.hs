{-# LANGUAGE DataKinds #-} -- promote data constructors into type constructors
{-# LANGUAGE GADTs #-} --extra type information 
{-# LANGUAGE KindSignatures #-} -- kinds in signatures
{-# LANGUAGE TypeFamilies #-} -- functions that operate on types
--{-# LANGUAGE UndecidableInstances #-} -- Add type family 

data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat

data Vector (n :: Nat) (a :: *) where
    VNil  :: Vector 'Zero a
    VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Show a => Show (Vector n a) where
    show VNil = "VNil"
    show (VCons a as) = "VCons " ++ show a ++ " (" ++ show as ++ ")"

type family Add n m where
    Add 'Zero n = n
    -- Add ('Succ n) m = Add n ('Succ m) -- GHC can't infer this is finite so we need UndecidableInstances
    Add ('Succ n) m = 'Succ (Add n m)


-- api 
append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil rest = rest
append (VCons a as) rest = VCons a $ append as rest 
