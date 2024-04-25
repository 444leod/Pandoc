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
    , printXML
) where

import ParserLib
import Debug.Trace

{- | XMLValue data type

    Represents an XML value
-}
data XMLValue = XMLValue {
    _name :: String,
    _attributes :: [(String, String)],
    _childrens :: [XMLChild]
} deriving (Show)

{- | XMLChild data type

    Represents an XML child
    Can either be a text or a node
-}
data XMLChild =
    XMLText String |
    XMLNode XMLValue deriving (Show)

{- | parseXMLValue function
    
    Parse an XML value
    Return an XMLValue or Nothing
-}
parseXMLValue :: Parser XMLValue
parseXMLValue = Parser $ \str ->
    case str of
        ('<':rest) -> do
            (name, rest') <-
                runParser (expectNoSeparators " \t\n" *> parseName) rest
            (attributes, rest'') <- runParser parseAttributes rest'
            (childs, rest''') <- runParser (parseChildrens <*
                parseEndToken name) rest''
            Just (XMLValue name attributes childs, rest''')
        _ -> Nothing

{- | parseName function

    Parse the name of a tag
    Return a String or Nothing
-}
parseName :: Parser String
parseName = Parser $ \str ->
    runParser (removePadding *> parseUntilChars " \t\n>") str

{- | parseAttributes function
    
    Parse the attributes of a tag
    Return a list of attributes or Nothing
-}
parseAttributes :: Parser [(String, String)]
parseAttributes = Parser $ \str -> do
    (_, stopRest) <- runParser removePadding str
    case stopRest of
        ('>':stopRest') -> Just ([], stopRest')
        _ -> runParser insideParseAttributes str

{- | insideParseAttributes function

    Parse the attributes of a tag
    Return a list of attributes or Nothing
-}
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

{- | parseChildrens function
    
    Parse the children of a tag
    Return a list of children or Nothing
-}
parseChildrens :: Parser [XMLChild]
parseChildrens = Parser $ \str -> case str of
    [] -> Nothing
    ('<':'/':rest) -> Just ([], rest)
    ('<':_) -> runParser parseXMLValue str >>= \(child, rest')
        -> runParser parseChildrens rest' >>= \(childrens, rest'')
        -> Just (XMLNode child:childrens, rest'')
    _ -> runParser subParseChildrens str 

{- | subParseChildrens

    Exist to reduce the complexity of parseChildren
-}
subParseChildrens :: Parser [XMLChild]
subParseChildrens = Parser $ \str -> do
    (text, rest') <- runParser parseText str
    (childrens, rest'') <- runParser parseChildrens rest'
    case text of
        [] -> Just (childrens, rest'')
        _ -> Just (XMLText text:childrens, rest'')

{- | parseText function
    
    Parse a text
    Return a String or Nothing
-}
parseText :: Parser String
parseText = Parser $ \str -> case runParser removePadding str of
    Just(_, '<':rest) -> Just ("", '<':rest)
    _ -> runParser (parseUntilChars "<") str

{-
    Parse the end of a tag
    Return () or Nothing
-}
parseEndToken :: String -> Parser ()
parseEndToken name = Parser $ \str -> case str of
    (' ':_) -> Nothing
    ('\n':_) -> Nothing
    ('\t':_) -> Nothing
    _ -> do (str', rest) <- runParser (parseName <* removePadding) str
            case (str' == name, rest) of
                (True, '>':rest') -> Just ((), rest')
                _ -> Nothing

{- | printXML function

    Return a String containing the XML value
-}
printXML :: XMLValue -> String
printXML (XMLValue name attributes childrens) =
    "<" ++ name ++ printAttributes attributes ++ ">" ++
    printChildrens childrens ++ "</" ++ name ++ ">"

{- | printAttributes function

    Return a String containing the attributes of a tag
-}
printAttributes :: [(String, String)] -> String
printAttributes [] = ""
printAttributes ((name, value):attributes) =
    " " ++ name ++ "=\"" ++ value ++ "\"" ++ printAttributes attributes

{- | printChildrens function

    Return a String containing the childrens of a tag
-}
printChildrens :: [XMLChild] -> String
printChildrens [] = ""
printChildrens (XMLText text:childrens) = text ++ printChildrens childrens
printChildrens (XMLNode node:childrens) =
    printXML node ++ printChildrens childrens
