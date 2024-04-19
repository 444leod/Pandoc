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

data XMLValue = XMLValue {
    _name :: String,
    _attributes :: [(String, String)],
    _childrens :: [XMLChild]
} deriving (Show)

data XMLChild =
    XMLText String |
    XMLNode XMLValue deriving (Show)

-- parseXMLValue :: Parser XMLValue
-- parseXMLValue = Parser $ \str ->
--     case str of
--         ('<':rest) -> do
--             (name, rest') <- runParser parseName rest
--             (attributes, rest'') <- runParser parseAttributes rest'
--             (childrens, rest''') <- runParser parseChildrens rest''
--             Just (XMLValue name attributes childrens, rest''')
--         _ -> Nothing

parseUntilChars :: String -> Parser String
parseUntilChars chars = Parser $ \str -> Just (span (`notElem` chars) str)

parseName :: Parser String
parseName = Parser $ \str ->
    runParser (removePadding *> parseUntilChars " \t\n") str

parseAttributes :: Parser [(String, String)]
parseAttributes = Parser $ \str ->
    case str of
        ('>':rest) -> Just ([], rest)
        _ -> do
            (name, rest') <-runParser (removePadding *>
                parseUntilChars "= \t\n") str
            (value, rest'') <- runParser (removePadding *> parseChar '=' *>
                removePadding *> parseString <* removePadding) rest'
            (attributes, rest''') <- runParser parseAttributes rest''
            Just ((name, value):attributes, rest''')
