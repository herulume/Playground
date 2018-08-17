import Data.Char
import Text.Read
import Control.Applicative

newtype Fix f = In { out :: f (Fix f) }
type Algebra f a = f a -> a
type CoAlgebra a f = a -> f a 

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . (fmap (cata f)) . out

ana :: Functor f => CoAlgebra a f -> a -> Fix f 
ana f = In . (fmap (ana f)) . f

hylo :: Functor f => Algebra f b -> CoAlgebra a f -> a -> b
hylo alg coalg = alg . (fmap (hylo alg coalg)) . coalg

-----------------------------------------------------------
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
----------------------------------------------------

possTerm :: String -> [String]
possTerm = parseString [] ['+', '*'] . stripspace  where
    parseString :: [String] -> String -> String -> [String]
    parseString acc ops [] = acc
    parseString acc ops [x] = acc
    parseString acc ops [x,y] = acc
    parseString acc ops (x:operand:x0:xs) | operand `elem` ops = parseString ([x,operand,x0]:acc) ops xs
                                          | otherwise = parseString acc ops (x0:xs)
stripspace :: String -> String
stripspace = foldr (\x acc -> case (isSpace x) of True -> acc; False -> x:acc) [] 

validTerms :: [String] -> Maybe AST
validTerms [] = Nothing
validTerms ([x,op,x0]:ts) = let (l, r) = (\y z -> (readMaybe [y] :: Maybe Int , readMaybe [z] :: Maybe Int)) x x0
                            in toAST op l r where
                            toAST :: Char -> Maybe Int -> Maybe Int -> Maybe AST
                            toAST op (Just a) (Just b) | op == '+' = Just $ toAdd a b
                                                       | op == '*' = Just $ toMult a b
                                                       | otherwise = Nothing
                            toAST _ Nothing _ = Nothing
                            toAST _ _ Nothing = Nothing

parse :: String -> Maybe AST
parse = validTerms . possTerm


main :: IO ()
main = getLine >>= \x -> case x of
                             "exit" -> pure ()
                             _ -> (\l -> case parse l of
                                            Nothing -> main
                                            (Just ast) -> (putStrLn . show . interpret) ast) x >> main
