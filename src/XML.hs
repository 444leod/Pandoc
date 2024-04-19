{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- XML
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module XML
    ( XMLValue(..)
) where

import ParserLib

import Debug.Trace

data XMLValue = XMLValue {
    _name :: String,
    _attributes :: [(String, String)],
    _childrens :: [XMLChild]
} deriving (Show)

data XMLChild =
    XMLText String |
    XMLNode XMLValue deriving (Show)

parseXMLValue :: Parser XMLValue
parseXMLValue = Parser $ \str ->
    case str of
        ('<':rest) -> do
            (name, rest') <-
                runParser (expectNoSeparators " \t\n" *> parseName) rest
            (attributes, rest'') <- runParser parseAttributes rest'
            -- (childrens, rest''') <- runParser parseChildrens rest''
            Just (XMLValue name attributes [], rest'')
        _ -> Nothing

parseName :: Parser String
parseName = Parser $ \str ->
    runParser (removePadding *> parseUntilChars " \t\n>") str

parseAttributes :: Parser [(String, String)]
parseAttributes = Parser $ \str -> do
    (_, stopRest) <- runParser removePadding str
    case stopRest of
        ('>':stopRest') -> Just ([], stopRest')
        _ -> runParser insideParseAttributes str

insideParseAttributes :: Parser [(String, String)]
insideParseAttributes = Parser $ \str ->
    case str of
        ('>':rest) -> Just ([], rest)
        _ -> do
            (name, rest') <- runParser (expectSeparators " \t\n" *>
                removePadding *> parseUntilChars "= \t\n") str
            (value, rest'') <- runParser (removePadding *> parseChar '=' *>
                removePadding *> parseString <* removePadding) rest'
            (attributes, rest''') <- runParser insideParseAttributes rest''
            Just ((name, value):attributes, rest''')
