{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Main
-}

module Main(main) where

import System.Environment(getArgs)
import Json (parseJsonValue, printJson)
import XML (parseXMLValue)
import JsonToDocument (jsonToDocument)
import DocumentToJson (documentToJson)
import ParserLib (runParser)
import Config
import Launcher

main :: IO ()
main = do
    args <- getArgs
    let conf = defaultConf
    let option = getOpts conf args
    validateConf option
    case option of
        Just opt -> launchFile (createVerifiedConf opt)
        Nothing -> myError "Error: invalid arguments"
