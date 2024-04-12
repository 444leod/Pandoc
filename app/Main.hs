{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Main
-}

module Main(main) where

import JsonParser (parseJsonValue)
import ParserLib (runParser)

main :: IO ()
main = print (runParser parseJsonValue "{\"key\": 42}")
