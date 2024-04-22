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
printContent (CParagraph (Paragraph paragraphContent)) _ =
    printParagraph paragraphContent ++ "\n"
printContent (CList list) _ = ""
printContent (CLink link) _ = printLink link
printContent (CImage image) _ = printImage image
printContent (CCodeBlock codeBlock) _ = printCodeBlock codeBlock

printSection :: String -> [Content] -> Int -> String
printSection [] content depth = printBody content depth
printSection title content depth =
    depthToHashtags depth ++ " " ++ title ++ "\n\n" ++
    printBody content depth

printParagraph :: [ParagraphContent] -> String
printParagraph content =
    concatMap printParagraphContent content ++ "\n"

printParagraphContent :: ParagraphContent -> String
printParagraphContent (PImage image) = printImage image
printParagraphContent (PTextFormat text) = printTextFormat text
printParagraphContent (PLink link) = printLink link

printImage :: Image -> String
printImage image = case _imgText image of
    FormatList imgText ->
        "![" ++ printFormatList imgText ++ "](" ++ _imgURL image ++ ")"

printLink :: Link -> String
printLink link = case _linkText link of
    FormatList linkText ->
        "[" ++ printFormatList linkText ++ "](" ++ _linkURL link ++ ")"

printCodeBlock :: CodeBlock -> String
printCodeBlock codeBlock =
    "```\n" ++
    printParagraphList codeBlock ++
    "```\n"

printParagraphList :: CodeBlock -> String
printParagraphList (CodeBlock paragraphs) =
    concatMap (printParagraph . getParagraphContents) paragraphs
    where
        getParagraphContents (Paragraph contents) = contents

printFormatList :: [Format] -> String
printFormatList = concatMap printTextFormat

printTextFormat :: Format -> String
printTextFormat (Bold text) = "**" ++ printTextFormat text ++ "**"
printTextFormat (Italic text) = "*" ++ printTextFormat text ++ "*"
printTextFormat (Code text) = "`" ++ printTextFormat text ++ "`"
printTextFormat (FContent text) = text

depthToHashtags :: Int -> String
depthToHashtags depth = replicate depth '#'
