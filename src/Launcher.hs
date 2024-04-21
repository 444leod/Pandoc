{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Launcher
-}

module Launcher
    ( launchFile
) where

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
    fileContent <- readFile (_iFile conf)
    case runParser parseJsonValue fileContent of
        Just (json, _) -> case jsonToDocument json of
            Just jsonDoc -> print (printJson (documentToJson jsonDoc))
            _ -> myError "Error: json is not a valid document"
        _ -> myError "Error: invalid json"
    return ()