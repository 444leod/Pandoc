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
import XML (parseXMLValue, printXML, XMLValue(..))
import MarkdownPrinter (printMarkdown)
import JsonToDocument (jsonToDocument)
import DocumentToJson (documentToJson)
import ParserLib (runParser, Parser, (<|>))
import Config
import Document

import Debug.Trace
import Control.Exception

{- | Parsable data type

    Represents a the Values that can be parsed
    ERRORVALUE is used to represent an error
    UNKNOWNVALUE is for when the format is not known and need to be inferred
-}
data Parsable =
    JSONVALUE JsonValue |
    XMLVALUE XMLValue |
    ERRORVALUE String |
    UNKNOWNVALUE Parsable deriving (Show)

{- | launchFile function

    Read a file, and gives it to launchParser
-}
launchFile :: VerifiedConf -> IO ()
launchFile conf = do
    fileContent <- getFileContents (_iFile conf)
    case fileContent of
        Nothing -> myError "Error: file not found"
        Just fileContent' -> launchParser conf fileContent'
    return ()

{- | launchParser function
    
    Parse the content based on conf and gives the result to launchDocument
-}
launchParser :: VerifiedConf -> String -> IO ()
launchParser conf fileContent = do
    parser <- chooseParser (_iFormat conf)
    case runParser parser fileContent of
        Nothing -> myError "Error: invalid file content"
        Just val -> launchDocument conf (fst val)
    return ()

{- | launchDocument function

    Convert the parsable to a document and gives it to launchPrinter 
-}
launchDocument :: VerifiedConf -> Parsable -> IO ()
launchDocument conf parsable = do
    maybeDoc <- convertToDocument parsable
    case maybeDoc of
        Nothing -> myError "Error: cannot convert to document"
        Just doc -> launchPrinter (_oFormat conf) (_oFile conf) doc

{- | launchPrinter function

    Print the document based on the format
-}
launchPrinter :: ConfFormat -> String -> Document -> IO ()
launchPrinter JSON "" doc = putStrLn (printJson (documentToJson doc))
launchPrinter JSON outfile doc =
    writeFileContents outfile (printJson (documentToJson doc))
launchPrinter XML "" _ = putStrLn "XML PRINT IS NOT IMPLEMENTED YET"
launchPrinter XML outfile _ =
    writeFileContents outfile "XML PRINT IS NOT IMPLEMENTED YET"
launchPrinter MARKDOWN "" doc = putStrLn (printMarkdown doc)
launchPrinter MARKDOWN outfile doc =
    writeFileContents outfile (printMarkdown doc)
launchPrinter _ _ _ = myError "Error: Output type is not supported"

{- | chooseParser function

    Return the parser based on the format
-}
chooseParser :: ConfFormat -> IO (Parser Parsable)
chooseParser JSON = return (JSONVALUE <$> parseJsonValue)
chooseParser XML = return (XMLVALUE <$> parseXMLValue)
chooseParser UNKNOWN = return (UNKNOWNVALUE <$> parseUnknown)
chooseParser _ = myError "Error: Format Not supported"
    >> return (return (ERRORVALUE "This will never get executed"))

{- | parseUnknown function

    Parse the content as a JSON or XML value
    Return a Parsable
-}
parseUnknown :: Parser Parsable
parseUnknown =
    JSONVALUE <$> parseJsonValue <|>
    XMLVALUE <$> parseXMLValue

{- | convertToDocument function

    Convert a Parsable to a Document
    Return a Maybe Document
-}
convertToDocument :: Parsable -> IO (Maybe Document)
convertToDocument (JSONVALUE x) = return (jsonToDocument x)
convertToDocument (XMLVALUE _) = return Nothing
convertToDocument (UNKNOWNVALUE (JSONVALUE x)) = return (jsonToDocument x)
convertToDocument (UNKNOWNVALUE (XMLVALUE _)) = return Nothing
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

{- | writeFileContents function

    Acts as a wrapper for the writeFile function
-}
writeFileContents :: String -> String -> IO ()
writeFileContents outfile content = catch (writeFile outfile content) handler
    where
        handler :: IOException -> IO ()
        handler _ = myError "Error: cannot write to file"