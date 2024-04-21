{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Main
-}

module Main(main) where

import System.Environment(getArgs)
import Json (parseJsonValue, printJson)
import XML (parseXMLValue, printXMLValue)
import JsonToDocument (jsonToDocument)
import DocumentToJson (documentToJson)
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
    case runParser parseXMLValue fileContent of
        Just (xml, _) -> print (printXMLValue xml)
        _ -> myError "Error: invalid xml"
    return ()