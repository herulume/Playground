module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- (<|>) :: Maybe a -> Maybe a -> Maybe a
-- Just x <|> _ = Just x
-- Nothing <|> o = o
