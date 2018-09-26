{-# LANGUAGE DataKinds #-} -- promote data constructors into type constructors
{-# LANGUAGE GADTs #-} --extra type information 
{-# LANGUAGE KindSignatures #-} -- kinds in signatures
{-# LANGUAGE TypeFamilies #-} -- functions that operate on types


data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat

data Vector (n :: Nat) (a :: *) where
    VNil  :: Vector 'Zero a
    (:-) :: a -> Vector n a -> Vector ('Succ n) a
infixr 5 :-

instance Show a => Show (Vector n a) where
    show VNil = "VNil"
    show (a :- as) = show a ++ " :- " ++ show as 

type family Add (n :: Nat) (m :: Nat)  where
    Add 'Zero n = n
    Add ('Succ n) m = 'Succ (Add n m)


-- api 
vappend :: Vector n a -> Vector m a -> Vector (Add n m) a
vappend VNil rest = rest
vappend (a :- as) rest = a :- vappend as rest 

vhead :: Vector (Succ n) a -> a
vhead (a :-  _ ) = a

toList :: Vector n a -> [a]
toList VNil = []
toList (a :- as) = a : toList as

data SafeList a where
    SafeList :: Vector n a -> SafeList a
instance Show a => Show (SafeList a) where
    show (SafeList  a) = "LOL: " ++ show a


vfromList :: [a] -> SafeList a
vfromList = foldr f (SafeList VNil) where
    f x (SafeList xs) = SafeList (x :- xs)
