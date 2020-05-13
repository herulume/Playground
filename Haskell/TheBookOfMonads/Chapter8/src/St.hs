{-# LANGUAGE ScopedTypeVariables #-}
module St where

import Control.Monad.ST
import Data.STRef
import Control.Exception

weirdSum = do
    x <- newSTRef 1
    y <- newSTRef 1
    modifySTRef y (+1)
    (+) <$> readSTRef x <*> readSTRef y

-- notAllowed = let var = runST (newSTRef 0)
--               in runST (readSTRef var)


divBy0 :: Int -> Int -> IO ()
divBy0 amount nP = print (amount `div` nP) `catch` (\(e :: ArithException) -> putStrLn "NUMERO ERRADO DE PESSOAS")


divBy0' :: Int -> Int -> IO ()
divBy0' amount nP = handle (\(e :: ArithException) -> putStrLn "NUMERO ERRADO DE PESSOAS") $
                            print (amount `div` nP)
