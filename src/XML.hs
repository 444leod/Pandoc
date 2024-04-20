{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- XML
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use if" #-}

module XML
    ( XMLValue(..)
    , parseXMLValue
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
            (name, rest') <- trace (show str) $
                runParser (expectNoSeparators " \t\n" *> parseName) rest
            (attributes, rest'') <- runParser parseAttributes rest'
            (childs, rest''') <- runParser (parseChildrens <*
                parseEndToken name) rest''
            Just (XMLValue name attributes childs, rest''')
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
                removePadding *> parseString) rest'
            (attributes, rest''') <- runParser insideParseAttributes rest''
            Just ((name, value):attributes, rest''')

parseChildrens :: Parser [XMLChild]
parseChildrens = Parser $ \str -> case str of
    [] -> Nothing
    ('<':'/':rest) -> Just ([], rest)
    ('<':_) -> runParser parseXMLValue str >>= \(child, rest')
        -> runParser parseChildrens rest' >>= \(childrens, rest'')
        -> Just (XMLNode child:childrens, rest'')
    _ -> do
        (text, rest') <- runParser parseText str
        (childrens, rest'') <- runParser parseChildrens rest'
        Just (XMLText text:childrens, rest'')

parseText :: Parser String
parseText = Parser $ \str -> runParser (parseUntilChars "<") str

parseEndToken :: String -> Parser ()
parseEndToken name = Parser $ \str -> case str of
    (' ':_) -> Nothing
    ('\n':_) -> Nothing
    ('\t':_) -> Nothing
    _ -> do (str', rest) <- runParser (parseName <* removePadding) str
            case (str' == name, rest) of
                (True, '>':rest') -> Just ((), rest')
                _ -> Nothing
