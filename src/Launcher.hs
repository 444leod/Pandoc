{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Launcher
-}

module Launcher
    ( launchFile
) where

import Json (parseJsonValue, printJson, JsonValue(..))
import JsonToDocument (jsonToDocument)
import DocumentToJson (documentToJson)
import ParserLib (runParser, Parser, (<|>))
import Config
import Document

import Debug.Trace
import Control.Exception

data Parsable =
    JSONVALUE JsonValue |
    XMLVALUE String | --tmp XML
    ERRORVALUE String |
    UNKNOWNEDVALUE Parsable deriving (Show)

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
        Just val -> launchDocument conf (fst val)
    return ()

launchDocument :: VerifiedConf -> Parsable -> IO ()
launchDocument conf parsable = do
    maybeDoc <- convertToDocument parsable
    case maybeDoc of
        Nothing -> myError "Error: cannot convert to document"
        Just doc -> launchPrinter (_oFormat conf) (_iFile conf) doc

launchPrinter :: Config.Format -> String -> Document -> IO ()
launchPrinter JSON outfile doc = print (printJson (documentToJson doc))
launchPrinter XML outfile doc = return ()
launchPrinter MARKDOWN outfile doc = return ()
launchPrinter _ outfile doc = myError "Error: Output type is not supported"

{- | chooseParser function

    Return the parser based on the format
-}
chooseParser :: Config.Format -> IO (Parser Parsable)
chooseParser JSON = return (JSONVALUE <$> parseJsonValue)
chooseParser XML = return (XMLVALUE <$> return "tmp XML")
chooseParser UNKNOWNED = return (UNKNOWNEDVALUE <$> parseUnknowned)
chooseParser _ = myError "Error: Format Not supported"
    >> return (return (ERRORVALUE "This will never get executed"))

parseUnknowned :: Parser Parsable
parseUnknowned =
    JSONVALUE <$> parseJsonValue <|>
    XMLVALUE <$> return "tmp XML"

convertToDocument :: Parsable -> IO (Maybe Document)
convertToDocument (JSONVALUE x) = return (jsonToDocument x)
convertToDocument (XMLVALUE _) = return Nothing
convertToDocument (UNKNOWNEDVALUE (JSONVALUE x)) = return (jsonToDocument x)
convertToDocument (UNKNOWNEDVALUE (XMLVALUE _)) = return Nothing
convertToDocument _ = return Nothing

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
