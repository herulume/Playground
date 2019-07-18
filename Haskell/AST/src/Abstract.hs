module Abstract 
    ( Op(..)
    , AST
    , toNum, toAdd, toMult
    , interpret, prettyPrint
    ) where

import RecursiveSchemes

data Op = Add | Mult deriving Show
data ASTF r = BinOpF Op r r | Num Int 
type AST = Fix ASTF 

instance Functor ASTF where
    fmap f (BinOpF op e d) = BinOpF op (f e) (f d)
    fmap _ (Num a) = (Num a)  


toNum :: Int -> AST
toNum = In . Num

toOp :: Op -> Int -> Int -> AST
toOp op l d = In $ BinOpF op (toNum l) (toNum d)

toAdd :: Int -> Int -> AST
toAdd l d = toOp Add l d

toMult :: Int -> Int -> AST
toMult l d = toOp Mult l d

interpret :: AST ->  Int 
interpret = cata algebraI  where
    algebraI :: ASTF Int -> Int
    algebraI (Num n) = n
    algebraI (BinOpF Add a b) = a + b
    algebraI (BinOpF Mult a b) = a * b

prettyPrint :: AST -> String 
prettyPrint = cata algebraS where
    algebraS :: ASTF String -> String
    algebraS (Num n) = show n
    algebraS (BinOpF Add a b) = "(" ++ a ++ " + " ++  b ++ ")"
    algebraS (BinOpF Mult a b) = a ++ " * " ++   b
