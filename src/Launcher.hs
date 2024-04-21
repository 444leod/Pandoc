{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Launcher
-}

module Launcher
    ( launchFile
) where

import Control.Exception
import Json (parseJsonValue, printJson, JsonValue(..))
import JsonToDocument (jsonToDocument)
import DocumentToJson (documentToJson)
import ParserLib (runParser, Parser)
import Config

data Parsable = JSONVALUE JsonValue | XMLVALUE String --tmp XML

{- | launchFile function

    Read a file, parse it and print the result based on the configuration
-}
launchFile :: VerifiedConf -> IO ()
launchFile conf = do
    fileContent <- getFileContents (_iFile conf)
    case fileContent of
        Nothing -> myError "Error: file not found"
        Just fileContent' -> launchParser conf fileContent'
    return ()

launchParser :: VerifiedConf -> String -> IO ()
launchParser conf fileContent = do
    parser <- chooseParser (_iFormat conf)
    case runParser parser fileContent of
        Nothing -> myError "Error: invalid file content"
        _ -> myError "GOOD, TMP ERROR"
    return ()

-- chooseParser :: Format -> IO (Parser a)
chooseParser :: Format -> IO (Parser Parsable)
chooseParser JSON = return (JSONVALUE <$> parseJsonValue)
chooseParser XML = return (XMLVALUE <$> return "tmp XML")
chooseParser _ = myError "Error: Format Not supported"
    >> return (XMLVALUE <$> return "This will never get executed")

{- | getFileContents function

    Acts as a wrapper for the readFile function
    Return the file content if it exists, Nothing otherwise
-}
getFileContents :: String -> IO (Maybe String)
getFileContents [] = return Nothing
getFileContents path = catch (fmap Just (readFile path)) handler
    where
        handler :: IOException -> IO (Maybe String)
        handler _ = return Nothing
