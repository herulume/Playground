{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Error where

class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

instance MonadError () Maybe where
    throwError () = Nothing
    catchError Nothing f = f ()
    catchError x _ = x

instance MonadError e (Either e) where
    throwError = Left
    Left l `catchError` h = h l
    Right r `catchError` _ = Right r

data NameReason = NameTooLong | UnknownCharacters deriving Show

type StringV = Either NameReason

vn :: String -> StringV String
vn name = validateName name `catchError` \case
      NameTooLong -> vn (cutName name)
      UnknownCharacters -> vn (normalize name)

validateName :: String -> StringV String
validateName name
  | length name > 10 = throwError NameTooLong
  | null name = throwError UnknownCharacters
  | otherwise = return name


cutName :: String -> String
cutName = take 10

normalize :: String -> String
normalize = const "TBA"

{-
return :: a -> Either e a       throwError :: e -> Either e a
(>>=) :: Either e a             catchError :: Either e a
      -> (a -> Either e b)                 -> (e -> Either f a)
      -> Either e b                        -> Either f a

Either is a Bimonad
-}
