-- Types def

newtype Fix f = In { out :: f (Fix f) } 

type Algebra f a = f a -> a

data Attr f a = Attr {attribute :: a, hole :: f (Attr f a) }
type CVAlgebra f a = f (Attr f a) -> a -- course-over-value 

-- Recursive schemes 

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . out

histo :: Functor f => CVAlgebra f a -> Fix f -> a 
histo h = h . fmap worker . out where
    worker        = toAttr .  split  . fmap worker  . out
    toAttr (a, b) = Attr a b
    split r       = (h r, r)

-- Nats def 

data NatF r = SF r | ZF 

instance Functor NatF where
    fmap _ ZF     = ZF
    fmap f (SF r) = SF $ f r

type Nat = Fix NatF  

-- Isomorphisms

natToInteger :: Nat -> Integer
natToInteger = cata $ \x -> case x of
    ZF     -> 0
    (SF x) -> x + 1

integerToNat :: Integer -> Nat
integerToNat 0 = In ZF
integerToNat n = In $ SF $ integerToNat (n-1)

-- Fib 

fibN :: Nat -> Integer 
fibN = histo $ \x -> case x of
    ZF     -> 1 
    (SF y) -> (attribute y) + (fromHisto . hole) y

fromHisto :: NatF (Attr NatF Integer) -> Integer 
fromHisto ZF     = 0
fromHisto (SF y) = attribute y

calcFib :: Integer -> Integer
calcFib = fibN . integerToNat
