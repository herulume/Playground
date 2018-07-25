newtype Fix f = Fix { unFix :: f (Fix f) }
data Op = Add | Mult deriving Show
data ASTF r = BinOpF Op r r | Num Int 
type AST = Fix ASTF 

instance Functor ASTF where
    fmap f (BinOpF op e d) = BinOpF op (f e) (f d)
    fmap _ (Num a) = (Num a)  

simpleExprFix :: Fix ASTF
simpleExprFix = Fix (BinOpF Mult (Fix (BinOpF Add (Fix (Num 1)) (Fix (Num 2)))) (Fix (Num 3)))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . (fmap (cata f)) . unFix

interpret :: AST ->  Int 
interpret = cata  algebraI 

algebraI :: ASTF Int -> Int
algebraI (Num n) = n
algebraI (BinOpF Add a b) = a + b
algebraI (BinOpF Mult a b) = a * b

algebraS :: ASTF String -> String
algebraS (Num n) = show n
algebraS (BinOpF Add a b) = "(" ++ a ++ " + " ++  b ++ ")"
algebraS (BinOpF Mult a b) = a ++ " * " ++   b

