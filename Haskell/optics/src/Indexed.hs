{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module Indexed where

import           Control.Lens
import           Control.Applicative
import           Data.Foldable
import           Data.Char
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import Data.List (stripPrefix)
import Data.Semigroup ((<>))


_Prefix :: String -> Prism' String String
_Prefix prefix = prism' embed match
    where
        match :: String -> Maybe String
        match = stripPrefix prefix
        embed :: String -> String
        embed s = prefix <> s



_Factor :: Int -> Prism' Int Int
_Factor n = prism' embed match
    where
        embed :: Int -> Int
        embed i = i * n
        match :: Int -> Maybe Int
        match i | i `mod` n == 0 = Just (i `div` n)
                | otherwise = Nothing

prismFizzBuzz :: Int -> String
prismFizzBuzz n | has (_Factor 3 . _Factor 5) n = "FizzBuzz"
                | has (_Factor 3) n = "Fizz"
                | has (_Factor 5) n = "Buzz"
                | otherwise = show n

runFizzBuzz :: IO ()
runFizzBuzz = for_ [1..20] $ \n -> putStrLn (prismFizzBuzz n)
