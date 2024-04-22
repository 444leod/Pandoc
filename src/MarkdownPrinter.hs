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
    printBody contents

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

printBody :: [Content] -> String
printBody = concatMap printContent

printContent :: Content -> String
printContent (CParagraph (Paragraph paragraphContent)) =
    printParagraph paragraphContent
printContent _ = ""


printParagraph :: [ParagraphContent] -> String
printParagraph = concatMap printParagraphContent

printParagraphContent :: ParagraphContent -> String
printParagraphContent (PTextFormat text) = printTextFormat text ++ "\n\n"
printParagraphContent _ = ""

printTextFormat :: Format -> String
printTextFormat (Bold text) = "**" ++ printTextFormat text ++ "**"
printTextFormat (Italic text) = "*" ++ printTextFormat text ++ "*"
printTextFormat (Code text) = "`" ++ printTextFormat text ++ "`"
printTextFormat (FContent text) = text

