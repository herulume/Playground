{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

data Val r = Val Int deriving Functor
data Add r = Add r r deriving Functor
data Mul r = Mul r r deriving Functor

infixr :+:
data (f :+: g) e = Inl (f e) | Inr (g e) deriving Functor 

type AstF = Val :+: Add :+: Mul

data Fix f = In (f (Fix f))

fold :: Functor f => (f a -> a) -> Fix f -> a
fold f (In t) = f (fmap (fold f) t) 

type AST = Fix AstF

class (Functor sub, Functor sup) => sub :<: sup where
        inj :: sub a -> sup a

instance Functor f => f :<: f where
         inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
         inj = Inl

instance {-# OVERLAPPING #-}(Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
          inj = Inr . inj


val :: (Val :<: f) => Int -> Fix f
val = In . inj . Val 

add :: (Add :<: f) => Fix f -> Fix f -> Fix f
add x y = In $ inj $ Add x y

mul :: (Mul :<: f) => Fix f -> Fix f -> Fix f
mul x y = In $ inj $ Mul x y

example :: (Add :<: f, Val :<: f) => Fix f
example =  add (val 4) (val 3)

class Functor f => Evaluate f where
        eval :: f Int -> Int

instance (Evaluate f, Evaluate g) => Evaluate (f :+: g) where
        eval (Inl x) = eval x
        eval (Inr x) = eval x

instance Evaluate Val where
        eval (Val w) = w
instance Evaluate Add  where
        eval (Add x y) = x + y 
instance Evaluate Mul where
        eval (Mul x y) = x * y


evalThis :: Evaluate f => Fix f -> Int 
evalThis = fold eval
