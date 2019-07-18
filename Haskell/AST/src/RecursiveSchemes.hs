module RecursiveSchemes where

newtype Fix f = In { out :: f (Fix f) }

type Algebra f a = f a -> a
type CoAlgebra f a = a -> f a 

type RAlgebra f a = f (Fix f, a) -> a
type RCoAlgebra f a = a -> f (Either (Fix f) a)

data Attr f a = Attr {attribute :: a, hole :: f (Attr f a) }
type CVAlgebra f a = f (Attr f a) -> a -- course-over-value

type DepAlgebra f a b = f (b, a) -> a
type DepAlgebra2 f a b = f (a, b) -> a


-- Folds
cata :: Functor f => Algebra f a -> Fix f -> a
-- write cata as a para
cata f = f . fmap (cata f) . out

para :: Functor f =>  RAlgebra f a  -> Fix f -> a -- look at current struct 
para f = f . fmap fanout . out where
    fanout t = (t, para f t)
--para f = snd . cata (\x -> (In $ fmap fst x, f x))

histo :: Functor f => CVAlgebra f a -> Fix f -> a
histo h = c where
    c = h . fmap worker . out
    worker = mkAttr .  helpr  . fmap worker  . out
    mkAttr (a, b) = Attr a b
    helpr r = (h r, r)

zygo :: Functor f => Algebra f b -> DepAlgebra f a b -> Fix f -> a -- semi mutual recursion
zygo f g = snd . cata (\x -> (f $ fmap fst x, g x))

mutu :: Functor f => DepAlgebra2 f b a -> DepAlgebra f a b-> Fix f -> a -- mutual recursion
mutu f g = snd . cata (\x -> (f x, g x))


-- UnFolds
ana :: Functor f => CoAlgebra f a -> a -> Fix f 
ana f = In . fmap (ana f) . f

apo :: Functor f => RCoAlgebra f a -> a -> Fix f
apo f = In . fmap (either id (apo f)) . f where

-- Both
hylo :: Functor f => Algebra f b -> CoAlgebra f a -> a -> b
-- hylo alg coalg = cata alg . ana coalg 2N
hylo alg coalg = alg . (fmap (hylo alg coalg)) . coalg -- N

