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

-- fails
-- only when priting the exception is raised
ae :: Rational -> Rational -> IO Rational
ae n d =
    return (n / d)
    `catch` \(e :: ArithException) -> do
                putStrLn $ "Numero errado de pessoas " ++ show e
                return 0

-- evaluate forces
ae' :: Rational -> Rational -> IO Rational
ae' n d =
    evaluate (n / d)
    `catch` \(e :: ArithException) -> do
                putStrLn $ "Numero errado de pessoas " ++ show e
                return 0
