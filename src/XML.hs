{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- XML
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Json
    ( XMLValue(..)
) where

import ParserLib
import Document

data XMLValue = XMLValue {
    name :: String,
    attributes :: [(String, String)],
    childrens :: [XMLChild]
} deriving (Show)

data XMLChild =
    XMLText String |
    XMLNode XMLValue deriving (Show)
