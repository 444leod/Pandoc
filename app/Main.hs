{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Main
-}

module Main(main) where

import System.Environment(getArgs)
import Json (parseJsonValue, printJson)
import JsonToDocument (jsonToDocument)
import ParserLib (runParser)
import Config

main :: IO ()
main = do
    args <- getArgs
    let conf = defaultConf
    let option = getOpts conf args
    validateConf option
    case option of
        Just opt -> launchFile (createVerifiedConf opt)
        Nothing -> myError "Error: invalid arguments"

-- TMP Function, needs rework
{- | launchFile function

    Read a file, parses it as a json, and print it as a json
-}
launchFile :: VerifiedConf -> IO ()
launchFile conf = do
    fileContent <- readFile (_iFile conf)
    case runParser parseJsonValue fileContent of
        Just (json, _) -> case jsonToDocument json of
            Just _ -> print (printJson json)
            _ -> myError "Error: json is not a valid document"
        _ -> myError "Error: invalid json"
    return ()