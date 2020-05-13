module BiFunctor where

class BiFunctor f where
    first :: (v -> w) -> f v a -> f w a
    second :: (a -> b) -> f v a -> f v b
    bimap :: (v -> w) -> (a -> b) -> f v a -> f w b
