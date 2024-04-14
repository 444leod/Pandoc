{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- ConfigLib
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE InstanceSigs #-}

module ParserLib
    ( parseChar
    , parseTryChar
    , parseAnyChar
    , parseExceptChar
    , parseOr
    , parseAnd
    , parseAndWith
    , parseMany
    , parseSome
    , parseUInt
    , parseInt
    , parseTuple
    , parseTruple
    , removePadding
    , parseString
    , Parser(..)
    , (>>=)
    , (<$>)
    , (<*>)
    , (<*)
    , (*>)
    , empty
    , (<|>)
    , pure
) where

import Control.Applicative(Alternative, empty, (<|>))

import Data.Bool()

import Control.Monad()

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
    {-  | fmap function
        Apply a function to the result of a parser
    -}
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap fct parser = Parser $ \str -> do
        (result, rest) <- runParser parser str
        return (fct result, rest)

instance Applicative Parser where
    {- | pure function
        Create a parser that always return the same value
    -}
    pure :: a -> Parser a
    pure a = Parser $ \str -> Just (a, str)

    {- | <*> function
        Apply a parser to the result of another parser
            while keeping the both result
    -}
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    parser1 <*> parser2 = Parser $ \str -> do
        (fct, rest1) <- runParser parser1 str
        (result, rest2) <- runParser parser2 rest1
        return (fct result, rest2)

    {- | <* function
        Apply a parser to the result of another parser
            while keeping the first result
    -}
    (<*) :: Parser a -> Parser b -> Parser a
    parser1 <* parser2 = Parser $ \str -> do
        (result1, rest1) <- runParser parser1 str
        (_, rest2) <- runParser parser2 rest1
        return (result1, rest2)

    {- | *> function
        Apply a parser to the result of another parser
            while keeping the second result
    -}
    (*>) :: Parser a -> Parser b -> Parser b
    parser1 *> parser2 = Parser $ \str -> do
        (_, rest1) <- runParser parser1 str
        (result2, rest2) <- runParser parser2 rest1
        return (result2, rest2)

instance Alternative Parser where
    {- | empty function
        Create a parser that always return Nothing
    -}
    empty :: Parser a
    empty = Parser $ const Nothing

    {- | <|> function
        Apply the first parser, if it fails apply the second one
    -}
    (<|>) :: Parser a -> Parser a -> Parser a
    parser1 <|> parser2 = Parser $ \str ->
        runParser parser1 str <|> runParser parser2 str

instance Monad Parser where
    {- | (>>=) function
        Sequentially compose two actions,
        discarding any value produced by the first, like sequencing operators
        (such as the semicolon) in imperative languages.
    -}
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    parser >>= fct = Parser $ \str -> do
        (result, rest) <- runParser parser str
        runParser (fct result) rest

{- | parseChar function
    Parse a character
    Return the character if it is the first character of the string
        and the rest of the string
    Return Nothing otherwise
-}
parseChar :: Char -> Parser Char
parseChar c = Parser $ \str ->
  case str of
    (x:xs)
        | x == c -> Just (c, xs)
        | otherwise -> Nothing
    _ -> Nothing

{- | parseTryChar function
    Try to parse a character
    Return the character as a string if it is the first character of the string
        and the rest of the string
    Return an empty string and the string otherwise 
-}
parseTryChar :: Char -> Parser String
parseTryChar c = Parser $ \str ->
  case str of
    (x:xs)
        | x == c -> Just ([c], xs)
        | otherwise -> Just ("", x:xs)
    _ -> Just ("", str)

{- | parseAnyChar function
    Parse any character in a list
    Return on of the character of the string if it is the first character
        of the string and the rest of the string
    Return Nothing otherwise
-}
parseAnyChar :: String -> Parser Char
parseAnyChar c = Parser $ \str ->
    case str of
        (x:xs)
            | x `elem` c -> Just (x, xs)
            | otherwise -> Nothing
        _ -> Nothing

{- | parseExceptChar function
    Parse any character except one
    Return the first character if it is not the given character of the string
        and the rest of the string
    Return Nothing otherwise
-}
parseExceptChar :: Char -> Parser Char
parseExceptChar c = Parser $ \str ->
  case str of
    (x:xs)
        | x == c -> Nothing
        | otherwise -> Just (x, xs)
    _ -> Nothing

{- | parseOr function
    Parse a character with two different parsers
    Return the result of the first parser if it is not Nothing
    Return the result of the second parser otherwise
-}
parseOr :: Parser a -> Parser a -> Parser a
parseOr parser1 parser2 = Parser $ \string ->
    case runParser parser1 string of
        Just (result, rest) -> Just (result, rest)
        Nothing -> runParser parser2 string

{- | parseAnd function
    Parse two characters
    Return a tuple of the result of the two parsers of the
        string and the rest of the string
    Return Nothing if one of the parser fails
-}
parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd parser1 parser2 = Parser $ \str -> do
    (result1, rest1) <- runParser parser1 str
    (result2, rest2) <- runParser parser2 rest1
    return ((result1, result2), rest2)

{- | parseAndWith function
    Same as parseAnd but with a function to apply to the two results
-}
parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
parseAndWith f parser1 parser2  = Parser $ \str -> do
    (result1, rest1) <- runParser parser1 str
    (result2, rest2) <- runParser parser2 rest1
    return (f result1 result2, rest2)

{- | parseMany function
    Parse a character multiple times
    Return a list of the result of the parser and the rest of the string
    Return an empty list and the string otherwise
-}
parseMany :: Parser a -> Parser [a]
parseMany parser1 = Parser $ \str ->
    case runParser parser1 str of
        Just (result1, rest1) -> do
            (result2, rest2) <- runParser (parseMany parser1) rest1
            return (result1 : result2, rest2)
        Nothing -> Just ([], str)

{- | parseSome function
    Parse a character multiple times
    Return a list of the result of the parser and the rest of the string
    Return Nothing and the string otherwise
-}
parseSome :: Parser a -> Parser [a]
parseSome parser1 = Parser $ \str -> do
    (result1, rest1) <- runParser parser1 str
    (result2, rest2) <- runParser (parseMany parser1) rest1
    return (result1 : result2, rest2)

{- | parseUInt function
    Parse an unsigned integer
    Return the integer and the rest of the string
    Return Nothing otherwise
-}
parseUInt :: Parser Int
parseUInt = Parser $ \str -> do
    (result, rest) <- runParser (parseSome (parseAnyChar ['0'..'9'])) str
    return (read result, rest)

{- | parseInt function
    Parse an signed integer
    Return the integer and the rest of the string
    Return Nothing otherwise
-}
parseInt :: Parser Int
parseInt = Parser $ \str -> do
    (neg, rest1) <- runParser (parseMany ( parseChar '-')) str
    (result, rest2) <- runParser (parseSome (parseAnyChar ['0'..'9'])) rest1
    return (read (neg ++ result), rest2)

{- | parseTuple function
    Parse a tuple
    Return the tuple and the rest of the string
    Return Nothing otherwise
-}
parseTuple :: Parser a -> Parser (a, a)
parseTuple parser1 = Parser $ \str ->
    case str of
        ('(':string) -> do
            (result1, rest1) <- runParser parser1 string
            (_, rest2) <- runParser (parseChar ',') rest1
            (result3, rest3) <- runParser parser1 rest2
            (_, rest4) <- runParser (parseChar ')') rest3
            return ((result1, result3), rest4)
        _ -> Nothing

{- | parseTruple function
    Parse a truple
    Return the truple and the rest of the string
    Return Nothing otherwise
-}
parseTruple :: Parser a -> Parser (a, a, a)
parseTruple parser1 = Parser $ \str -> do
    (_, rest1) <- runParser (parseChar '(') str
    (result2, rest2) <- runParser parser1 rest1
    (_, rest3) <- runParser (parseChar ',') rest2
    (result4, rest4) <- runParser parser1 rest3
    (_, rest5) <- runParser (parseChar ',') rest4
    (result6, rest6) <- runParser parser1 rest5
    (_, rest7) <- runParser (parseChar ')') rest6
    return ((result2, result4, result6), rest7)

{- | removePadding function
    Remove padding from a string
    Return the string without padding
-}
removePadding :: Parser String
removePadding = Parser $ \str ->
    runParser (parseMany (parseAnyChar " \t\r\n")) str

{- | parseString function
    Parse a string
    Return a string or Nothing
-}
parseString :: Parser String
parseString = Parser $ \str ->
    case str of
        ('"':'"':rest) -> return ("", rest)
        ('"':rest0) -> do
            (result, rest1) <- runParser (parseSome (parseExceptChar '"')) rest0
            (_, rest2) <- runParser (parseChar '"') rest1
            return (result, rest2)
        _ -> Nothing
