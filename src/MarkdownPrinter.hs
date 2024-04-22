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
printMarkdown doc = case _body doc of
  Body contents ->
    printHeader (_header doc) ++ "\n" ++
    printBody contents 0

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

printBody :: [Content] -> Int -> String
printBody content depth = concatMap (`printContent` depth) content

printContent :: Content -> Int -> String
printContent (CParagraph (Paragraph paragraphContent)) depth =
    printParagraph paragraphContent depth ++ "\n"
printContent (CSection (Section title content)) depth =
    printSection title content (depth + 1)
printContent _ _ = ""

printSection :: String -> [Content] -> Int -> String
printSection [] content depth = printBody content depth
printSection title content depth =
    depthToHashtags depth ++ " " ++ title ++ "\n\n" ++
    printBody content depth

printParagraph :: [ParagraphContent] -> Int -> String
printParagraph content depth = concatMap (`printParagraphContent` depth) content ++ "\n"

printParagraphContent :: ParagraphContent -> Int -> String
printParagraphContent (PTextFormat text) depth = printTextFormat text
printParagraphContent _ _ = ""

printTextFormat :: Format -> String
printTextFormat (Bold text) = "**" ++ printTextFormat text ++ "**"
printTextFormat (Italic text) = "*" ++ printTextFormat text ++ "*"
printTextFormat (Code text) = "`" ++ printTextFormat text ++ "`"
printTextFormat (FContent text) = text

depthToHashtags :: Int -> String
depthToHashtags depth = replicate depth '#'
