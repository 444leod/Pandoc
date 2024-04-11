{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- ConfigLib
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Lib
    ( parseChar
    , parseTryChar
    , parseAnyChar
    , Parser(..)
) where

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

parseChar :: Char -> Parser Char
parseChar c = Parser $ \str ->
  case str of
    (x:xs)
        | x == c -> Just (c, xs)
        | otherwise -> Nothing
    _ -> Nothing

parseTryChar :: Char -> Parser String
parseTryChar c = Parser $ \str ->
  case str of
    (x:xs)
        | x == c -> Just ([c], xs)
        | otherwise -> Just ("", x:xs)
    _ -> Just ("", str)

parseAnyChar :: String -> Parser Char
parseAnyChar c = Parser $ \str ->
    case str of
        (x:xs)
            | x `elem` c -> Just (x, xs)
            | otherwise -> Nothing
        _ -> Nothing

parseExceptChar :: Char -> Parser Char
parseExceptChar c = Parser $ \str ->
  case str of
    (x:xs)
        | x == c -> Nothing
        | otherwise -> Just (x, xs)
    _ -> Nothing