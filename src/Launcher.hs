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
import Json (parseJsonValue, printJson)
import JsonToDocument (jsonToDocument)
import DocumentToJson (documentToJson)
import ParserLib (runParser)
import Config

{- | launchFile function

    Read a file, parse it and print the result based on the configuration
-}
launchFile :: VerifiedConf -> IO ()
launchFile conf = do
    fileContent <- getFileContents (_iFile conf)
    case fileContent of
        Nothing -> myError "Error: file not found"
        Just fileContent' -> case runParser parseJsonValue fileContent' of
            Just (json, _) -> case jsonToDocument json of
                Just jsonDoc -> print (printJson (documentToJson jsonDoc))
                _ -> myError "Error: json is not a valid document"
            _ -> myError "Error: invalid json"
    return ()

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
