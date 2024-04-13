{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Main
-}

module Main(main) where

import System.Environment(getArgs)
import JsonParser (parseJsonValue)
import ParserLib (runParser)
import Config

main :: IO ()
main = do
    args <- getArgs
    let conf = defaultConf
    let option = getOpts conf args
    -- validateConf option
    -- case option of
    --     Just opt -> launchFile (createVerifiedConf opt)
    --     Nothing -> myError "Error: invalid arguments"