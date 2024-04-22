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
printContent (CSection (Section title content)) depth =
    printSection title content (depth + 1)
printContent (CParagraph (Paragraph paragraphContent)) depth =
    printParagraph paragraphContent depth ++ "\n"
printContent (CList list) depth = ""
printContent (CLink link) _ = ""
printContent (CImage image) _ = printImage image
printContent (CCodeBlock codeBlock) _ = ""

printSection :: String -> [Content] -> Int -> String
printSection [] content depth = printBody content depth
printSection title content depth =
    depthToHashtags depth ++ " " ++ title ++ "\n\n" ++
    printBody content depth

printParagraph :: [ParagraphContent] -> Int -> String
printParagraph content depth =
    concatMap (`printParagraphContent` depth) content ++ "\n"

printParagraphContent :: ParagraphContent -> Int -> String
printParagraphContent (PImage image) _ = printImage image
printParagraphContent (PTextFormat text) _ = printTextFormat text
printParagraphContent (PLink link) _ = ""

printImage :: Image -> String
printImage image = case _imgText image of
    FormatList imgText ->
        "![" ++ printFormatList imgText ++ "](" ++ _imgURL image ++ ")"

printFormatList :: [Format] -> String
printFormatList = concatMap printTextFormat

printTextFormat :: Format -> String
printTextFormat (Bold text) = "**" ++ printTextFormat text ++ "**"
printTextFormat (Italic text) = "*" ++ printTextFormat text ++ "*"
printTextFormat (Code text) = "`" ++ printTextFormat text ++ "`"
printTextFormat (FContent text) = text

depthToHashtags :: Int -> String
depthToHashtags depth = replicate depth '#'
