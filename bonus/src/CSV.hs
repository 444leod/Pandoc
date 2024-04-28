{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- CSVParser
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module CSV
    ( parseCSVValue
    , launchCSV
) where

import ParserLib
import Launcher
import Config

newtype CSVValue = CSVValue [CSVLine] deriving (Show)

newtype CSVLine = CSVLine [CSVItem] deriving (Show)

data CSVItem =
    Empty |
    Boolean Bool |
    Float Float |
    CSVWord String |
    CSVString String deriving (Show)

{- | parseBoolean function
    Parse a boolean value
    Return a Boolean CSVItem or Nothing
-}
parseBoolean :: Parser CSVItem
parseBoolean = Parser $ \str ->
    case str of
        ('t':'r':'u':'e':rest) -> Just (Boolean True, rest)
        ('T':'r':'u':'e':rest) -> Just (Boolean True, rest)
        ('y':'e':'s':rest) -> Just (Boolean True, rest)
        ('f':'a':'l':'s':'e':rest) -> Just (Boolean False, rest)
        ('F':'a':'l':'s':'e':rest) -> Just (Boolean False, rest)
        ('n':'o':rest) -> Just (Boolean False, rest)
        _ -> Nothing

{- | parseCSVString function
    Parse a string value
    Return a JString CSVItem or Nothing
-}
parseCSVString :: Parser CSVItem
parseCSVString = Parser $ \str ->
    case str of
        ('"':'"':rest) -> Just (CSVString "", rest)
        ('"':rest) -> do
            (result, rest1) <- runParser (parseSome (parseExceptChar '"')) rest
            (_, rest2) <- runParser (parseChar '"') rest1
            return (CSVString result, rest2)
        _ -> Nothing

{- | parseCSVWord function
    Parse an array value
    Return a JArray CSVItem or Nothing
-}
parseCSVWord :: Parser CSVItem
parseCSVWord = Parser $ \str ->
    case runParser (parseUntilChars " \n\t,") str of
        Just ("", rest) -> Just (Empty, rest)
        Just (result, rest) -> Just (CSVWord result, rest)
        Nothing -> Nothing

{- | parseFloatingNumber function
    Parse a number value
    Return a Number CSVItem or Nothing
-}
parseFloatingNumber :: Parser CSVItem
parseFloatingNumber = Parser $ \str ->
    case runParser parseFloat str of
        Just (result, rest) -> Just (Float result, rest)
        Nothing -> Nothing

{- | parseFloat function
    Parse a float value
    Return a Float CSVItem or Nothing
-}
parseFloat :: Parser Float
parseFloat = Parser $ \str -> do
    (res, rest) <- runParser (parseSome (parseAnyChar (['0'..'9'] ++ "."))) str
    return (read res, rest)

{- | parseCSVItem function
    Parse a CSVItem
    Return a CSVItem or Nothing
-}
parseCSVItem :: Parser CSVItem
parseCSVItem = removeSpaces *> (
    parseBoolean <|>
    parseFloatingNumber <|>
    parseCSVString <|>
    parseCSVWord
    ) <* removeSpaces

{- | parseCSVLine function
    Parse a CSVLine
    Return a CSVLine or Nothing
-}
parseCSVLine :: Parser CSVLine
parseCSVLine = Parser $ \str -> case runParser ((,) <$> parseMany
    (removeSpaces *> parseCSVItem <* parseChar ',') <*>
    (removeSpaces *> parseCSVItem)) str of
        Just ((res0, res1), rest) -> return (CSVLine (res0 ++ [res1]), rest)
        Nothing -> Nothing

{- | parseCSVValue function
    Parse a CSVValue
    Return a CSVValue or Nothing
-}
parseCSVValue :: Parser CSVValue
parseCSVValue = Parser $ \str -> case runParser (parseMany
    (parseChar '\n' *> parseCSVLine)) ("\n" ++ str) of
        Just (res, rest) -> return (CSVValue res, rest)
        Nothing -> Nothing

{- | removeSpaces function
    Remove spaces from a string
    Return the string without spaces
-}
removeSpaces :: Parser String
removeSpaces = Parser $ \str ->
    runParser (parseMany (parseAnyChar " \t")) str

{- | launchCSV function
    Launch the CSV parser
-}
launchCSV :: String -> String -> IO ()
launchCSV "" infile = do
    content <- getFileContents infile
    case content of
        Nothing -> myError "Error: file not found"
        Just content' -> print (runParser parseCSVValue content')
launchCSV outfile infile = do
    content <- getFileContents infile
    case content of
        Nothing -> myError "Error: file not found"
        Just content' ->
            writeFileContents outfile (show(runParser parseCSVValue content'))
