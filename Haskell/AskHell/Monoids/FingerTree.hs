-- FingerTree to implement an "Array"
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.FingerTree
import Data.Monoid

newtype Size a = Size { getSize :: a } deriving (Show, Eq)
instance Measured (Sum Int) (Size a) where
    measure _ = Sum 1

alphabet :: FingerTree (Sum Int) (Size Char)
alphabet = fromList $ Size <$> "abcdefghijklmnopqrstovwxyz"

atIndex :: Int -> FingerTree (Sum Int) (Size a) -> Maybe a
atIndex n t =
    case viewl . snd $ split ( > Sum n) t of
        Size c :< _ -> Just c
        _ -> Nothing
