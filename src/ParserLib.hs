{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- ConfigLib
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

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

parseOr :: Parser a -> Parser a -> Parser a
parseOr parser1 parser2 = Parser $ \string ->
    case runParser parser1 string of
        Just (result, rest) -> Just (result, rest)
        Nothing -> runParser parser2 string

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd parser1 parser2 = Parser $ \str -> do
    (result1, rest1) <- runParser parser1 str
    (result2, rest2) <- runParser parser2 rest1
    return ((result1, result2), rest2)

parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
parseAndWith f parser1 parser2  = Parser $ \str -> do
    (result1, rest1) <- runParser parser1 str
    (result2, rest2) <- runParser parser2 rest1
    return (f result1 result2, rest2)

parseMany :: Parser a -> Parser [a]
parseMany parser1 = Parser $ \str ->
    case runParser parser1 str of
        Just (result1, rest1) -> do
            (result2, rest2) <- runParser (parseMany parser1) rest1
            return (result1 : result2, rest2)
        Nothing -> Just ([], str)

parseSome :: Parser a -> Parser [a]
parseSome parser1 = Parser $ \str -> do
    (result1, rest1) <- runParser parser1 str
    (result2, rest2) <- runParser (parseMany parser1) rest1
    return (result1 : result2, rest2)

parseUInt :: Parser Int
parseUInt = Parser $ \str -> do
    (result, rest) <- runParser (parseSome (parseAnyChar ['0'..'9'])) str
    return (read result, rest)

parseInt :: Parser Int
parseInt = Parser $ \str -> do
    (neg, rest1) <- runParser (parseMany ( parseChar '-')) str
    (result, rest2) <- runParser (parseSome (parseAnyChar ['0'..'9'])) rest1
    return (read (neg ++ result), rest2)

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
