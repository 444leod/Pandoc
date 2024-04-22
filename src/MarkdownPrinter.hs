{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- MarkdownPrinter
-}

module MarkdownPrinter (
    printMarkdown
) where

import Document

printMarkdown :: Document -> String
printMarkdown doc = printHeader (_header doc)
    -- ++ printBody (_body(doc))

printHeader :: Header -> String
printHeader header =
    "---\n" ++
    "title: " ++ _title header ++ "\n" ++
    printAuthor (_author header) ++
    printDate (_date header) ++
    "---\n"

printAuthor :: Maybe String -> String
printAuthor Nothing = ""
printAuthor (Just author) = "author: " ++ author ++ "\n"

printDate :: Maybe String -> String
printDate Nothing = ""
printDate (Just date) = "date: " ++ date ++ "\n"
