module Profunctor where

class Profunctor f where
    lmap :: (v -> w) -> f w a -> f v a
    rmap :: (a -> b) -> f v a -> f v b
    dimap :: (v -> w) -> (a-> b) -> f w a -> f v b
