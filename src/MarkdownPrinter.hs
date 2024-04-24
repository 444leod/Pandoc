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

{- | printMarkdown

    Gives a string representation of a document in markdown format
-}
printMarkdown :: Document -> String
printMarkdown doc = case _body doc of
  Body contents ->
    printHeader (_header doc) ++ "\n" ++
    printBody contents 0

{- | printHeader

    Gives a string representation of a header in markdown format
-}
printHeader :: Header -> String
printHeader header =
    "---\n" ++
    "title: " ++ _title header ++ "\n" ++
    printAuthor (_author header) ++
    printDate (_date header) ++
    "---\n"

{- | printAuthor

    Gives a string representation of an author in markdown format
-}
printAuthor :: Maybe String -> String
printAuthor Nothing = ""
printAuthor (Just author) = "author: " ++ author ++ "\n"

{- | printDate

    Gives a string representation of a date in markdown format
-}
printDate :: Maybe String -> String
printDate Nothing = ""
printDate (Just date) = "date: " ++ date ++ "\n"

{- | printBody

    Gives a string representation of a body in markdown format
-}
printBody :: [Content] -> Int -> String
printBody content depth = concatMap (`printContent` depth) content

{- | printContent

    Gives a string representation of a content in markdown format
-}
printContent :: Content -> Int -> String
printContent (CSection (Section title content)) depth =
    printSection title content (depth + 1)
printContent (CParagraph (Paragraph paragraphContent)) _ =
    printParagraph paragraphContent ++ "\n"
printContent (CList list) _ = printList list 0
printContent (CLink link) _ = printLink link
printContent (CImage image) _ = printImage image
printContent (CCodeBlock codeBlock) _ = printCodeBlock codeBlock
printContent (CTextFormat text) _ = printTextFormat text ++ "\n"

{- | printSection

    Gives a string representation of a section in markdown format
-}
printSection :: String -> [Content] -> Int -> String
printSection [] content depth = printBody content depth
printSection title content depth =
    replicate depth '#' ++ " " ++ title ++ "\n\n" ++
    printBody content depth

{- | printParagraph

    Gives a string representation of a paragraph in markdown format
-}
printParagraph :: [ParagraphContent] -> String
printParagraph content =
    concatMap printParagraphContent content ++ "\n"

{- | printParagraphContent

    Gives a string representation of a paragraph content in markdown format
-}
printParagraphContent :: ParagraphContent -> String
printParagraphContent (PImage image) = printImage image
printParagraphContent (PTextFormat text) = printTextFormat text
printParagraphContent (PLink link) = printLink link

{- | printImage

    Gives a string representation of an image in markdown format
-}
printImage :: Image -> String
printImage image = case _imgText image of
    FormatList imgText ->
        "![" ++ printFormatList imgText ++ "](" ++ _imgURL image ++ ")"

{- | printLink

    Gives a string representation of a link in markdown format
-}
printLink :: Link -> String
printLink link = case _linkText link of
    FormatList linkText ->
        "[" ++ printFormatList linkText ++ "](" ++ _linkURL link ++ ")"

{- | printCodeBlock

    Gives a string representation of a code block in markdown format
-}
printCodeBlock :: CodeBlock -> String
printCodeBlock codeBlock =
    "```\n" ++
    subPrintCodeBlock codeBlock ++
    "```\n"

{- | subPrintCodeBlock

    Gives a string representation of the inside of a
    code block in markdown format
-}
subPrintCodeBlock :: CodeBlock -> String
subPrintCodeBlock (CodeBlock paragraphs) = case paragraphs of
    [] -> ""
    (x:xs) -> printCodeBlockContent x ++ subPrintCodeBlock (CodeBlock xs)

printCodeBlockContent :: CodeBlockContent -> String
printCodeBlockContent (CodeBlockParagraph (Paragraph paragraph)) =
    printParagraph paragraph
printCodeBlockContent (CodeBlockTextFormat text) = printTextFormat text ++ "\n"

{- | printList
    
    Gives a string representation of a list in markdown format
-}
printList :: List -> Int -> String
printList (List listContent) depth =
    concatMap (`printListContent` depth) listContent ++ getNewLineByDepth depth

{- | printListContent

    Gives a string representation of a list content in markdown format
-}
printListContent :: ListContent -> Int -> String
printListContent (LParagraph (Paragraph paragraph)) depth =
    replicate depth '\t' ++ "- " ++ printParagraph paragraph
printListContent (LTextFormat text) depth =
    replicate depth '\t' ++ "- " ++ printTextFormat text ++ "\n"
printListContent (SubList list) depth = printList list (depth + 1)

{- | printTextFormat

    Gives a string representation of a text format in markdown format
-}
printFormatList :: [Format] -> String
printFormatList = concatMap printTextFormat

{- | printTextFormat

    Gives a string representation of a text format in markdown format
-}
printTextFormat :: Format -> String
printTextFormat (Bold text) = "**" ++ printTextFormat text ++ "**"
printTextFormat (Italic text) = "*" ++ printTextFormat text ++ "*"
printTextFormat (Code text) = "`" ++ printTextFormat text ++ "`"
printTextFormat (FContent text) = text

{- | getNewLineByDepth

    Utility function to get a new line based on the depth
-}
getNewLineByDepth :: Int -> String
getNewLineByDepth 0 = "\n"
getNewLineByDepth _ = ""
