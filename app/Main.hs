{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Main
-}

module Main(main) where

import System.Environment(getArgs)
import JsonParser (parseJsonValue, printJson)
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
    readFile (_iFile conf) >>=
        putStrLn . maybe "Invalid Json"(printJson . fst) . runParser parseJsonValue
    return ()