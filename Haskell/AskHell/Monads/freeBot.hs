module FreeBot where

data Free f r = Free (f (Free f r)) -- Free Monad, f is language, r is the return value
              | Pure r

data DirectiveF r = L r | R r | S
type Directive a = Free DirectiveF a

left  = liftF (L ())
right = liftF (R ())
stop  = liftF S

interpretIO :: Directive a -> IO ()
interpretIO (Free (L f)) = putStrLn "Time to go left" >> interpretIO f
interpretIO (Free (R f)) = putStrLn "Going right" >> interpretIO f
interpretIO (Free S)     = putStrLn "Shutdow!"
interpretIO (Pure _)     = putStrLn "ERROR ERROR ERROR"


program0 :: Directive ()
program0 = do
  left
  right
  left
  stop

program1 :: Bool -> Directive ()
program1 goLeftAlways = do
  left
  if goLeftAlways
     then left
     else right
  left
  stop

main :: IO ()
main = interpretIO program0

data StackF k  = Push Int k
               | Top (Int -> k)
               | Pop k
               | Add k
               deriving Functor
push :: Int -> FreeStack ()
push n = liftF (Push n ())

pop :: FreeStack ()
pop = liftF (Pop ())

top :: FreeStack Int
top = liftF (Top id)

add :: FreeStack ()
add = liftF (Add ())
-- Since we are not using any library and/or language extension
instance Functor DirectiveF where
  fmap f (L r) = L $ f r
  fmap f (R r) = R $ f r
  fmap _ S     = S

instance Functor f => Functor (Free f) where
  fmap f (Pure a)  = Pure (f a)
  fmap f (Free fa) = Free (fmap f <$> fa)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b = Free $ (<*> b) <$> ma

instance (Functor f) => Monad (Free f) where
  return = Pure
  (Free x) >>= f = Free (fmap (>>= f) x)
  (Pure r) >>= f = f r

liftF :: (Functor f) => f r -> Free f r
liftF = Free . fmap Pure
