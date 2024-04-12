{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Main
-}

module Main(main) where

import ParserLib

main :: IO ()
main = print (runParser (parseChar 't') "test")
