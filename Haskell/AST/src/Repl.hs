module Repl
    ( repl
    ) where

import Data.Char
import Text.Read
import Abstract

possTerm :: String -> [String]
possTerm = parseString [] ['+', '*'] . stripspace  where
    parseString :: [String] -> String -> String -> [String]
    parseString acc ops [] = acc
    parseString acc ops [x] = acc
    parseString acc ops [x,y] = acc
    parseString acc ops (x:operand:x0:xs) | operand `elem` ops = parseString ([x,operand,x0]:acc) ops xs
                                          | otherwise = parseString acc ops (x0:xs)
stripspace :: String -> String
stripspace = foldr (\x acc -> case (isSpace x) of True -> acc; False -> x:acc) [] 

-- this is rubbish 
validTerms :: [String] -> Maybe AST
validTerms [] = Nothing
validTerms ([x,op,x0]:ts) = let (l, r) = (\y z -> (readMaybe [y] :: Maybe Int , readMaybe [z] :: Maybe Int)) x x0
                            in toAST op l r where
                            toAST :: Char -> Maybe Int -> Maybe Int -> Maybe AST
                            toAST op (Just a) (Just b) | op == '+' = Just $ toAdd a b
                                                       | op == '*' = Just $ toMult a b
                                                       | otherwise = Nothing
                            toAST _ Nothing _ = Nothing
                            toAST _ _ Nothing = Nothing

parse :: String -> Maybe AST
parse = validTerms . possTerm


repl :: IO ()
repl = getLine >>= \x -> case x of
                             "exit" -> pure ()
                             _ -> (\l -> case parse l of
                                            Nothing -> repl
                                            (Just ast) -> (putStrLn . show . interpret) ast) x >> repl
