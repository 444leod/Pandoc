{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Caclulator
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Calculator(
    expr, 
    term,
    factor
) where

import ParserLib
import Debug.Trace

expr :: Parser Int
expr = Parser $ \str -> do
    (x, rest) <- runParser (removePadding *> term <* removePadding) str
    case rest of
        '+':rest -> do
            (y, rest') <- runParser (removePadding *> expr) rest
            return (x + y, rest')
        '-':rest -> do
            (y, rest') <- runParser (removePadding *> expr) rest
            return (x - y, rest')
        _ -> runParser (removePadding *> term) str


term :: Parser Int
term = Parser $ \str -> do
    (x, rest) <- runParser (removePadding *> factor <* removePadding) str
    case rest of
        '*':rest -> do
            (y, rest') <- runParser (removePadding *> term) rest
            return (x * y, rest')
        '/':rest -> do
            (y, rest') <- runParser (removePadding *> term) rest
            return (x `div` y, rest')
        _ -> runParser (removePadding *> factor) str

factor :: Parser Int
factor = Parser $ \str -> 
    case runParser (removePadding *> parseChar '(' *> removePadding
        *> expr <* removePadding <* parseChar ')') str of
        Just (x, rest) -> return (x, rest)
        Nothing -> runParser (removePadding *> parseInt) str

