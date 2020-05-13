module Writer where

import Data.Monoid
import BiFunctor

newtype Writer w a =
    Writer { runWriter :: (w, a) }

instance Functor (Writer w) where
    fmap f (Writer (w, a)) = Writer (w, f a)

instance Monoid w => Applicative (Writer w) where
    pure a = Writer (mempty, a)
    (Writer (w, f)) <*> (Writer (w', a)) = Writer (w <> w', f a)

instance Monoid w => Monad (Writer w) where
    return = pure
    (Writer (w, a)) >>= amb = let (Writer (w', a')) = amb a
                               in Writer (w <> w', a')

tell :: w -> Writer w ()
tell w = Writer (w, ())

listen :: Writer w a -> Writer w (a, w)
listen (Writer (w, a)) = Writer (w, (a, w))

pass :: Writer w (a, w -> w) -> Writer w a
pass (Writer (w, (a, f))) = Writer (f w, a)

censor :: (w -> w) -> Writer w a -> Writer w a
censor f (Writer (w, a)) = Writer (f w, a)

example :: Writer (Sum Int) String
example = do
    tell (Sum 3)
    tell (Sum 4)
    return "seven"

example1 = do
    tell (Sum 2)
    listen $ do tell (Sum 3)
                tell (Sum 4)

data Expr = Lit Float | Add Expr Expr | Divide Expr Expr

eval :: Expr -> Float
eval (Lit n) = n
eval (Add e0 e1) = eval e0 + eval e1
eval (Divide e0 e1) = case (eval e0, eval e1) of
                        (_, 0) -> 0
                        (a, b) -> a / b

eval' :: Expr -> Writer [String] Float
eval' (Lit n) = pure n
eval' (Add e0 e1) = (+) <$> eval' e0 <*> eval' e1
eval' (Divide e0 e1) = do
    a <- eval' e0
    b <- eval' e1
    case (a, b) of
      (_, 0) -> tell ["Devide by 0!"] >> pure 0
      (u, v) -> pure $ u / v

e = Divide (Add (Lit 2) (Lit 3)) (Lit 0)

instance BiFunctor Writer where
    first f (Writer (w, a)) = Writer (f w, a)
    second f (Writer (w, a)) = Writer (w, f a)
    bimap f g (Writer (w, a)) = Writer (f w, g a)
