{-
-- EPITECH PROJECT, 2024
-- Pandoc
-- File description:
-- Markdown
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Markdown (
    MarkdownBlock(..),
    MarkdownInline(..),
    parseMdTitle,
    parseMdListNode,
    parseLineIndicator,
    parseMdHeaderInfo,
    parseMdParagraph
) where

import ParserLib

data MarkdownBlock =
    Paragraph [MarkdownInline] | -- a line of text
    HeaderInfo String String | -- a line of text
    ListNode [MarkdownInline] | -- a node of a list
    Title Int [MarkdownInline] | -- a header (the number is for the importance [1-4])
    CodeBlock [String] | -- a chunk of code on multiple lines
    LineIndicator -- an horizontal line
    deriving (Show)

data MarkdownFormat =
    Plain String | -- a word or chunck of words
    Bold MarkdownInline | -- adds a bold attribute to
    Italic MarkdownInline | -- adds italic attribute to chunk
    Code MarkdownInline
    deriving (Show)

data MarkdownInline =
    Text MarkdownFormat |
    Link MarkdownFormat String |
    Image MarkdownFormat String
    deriving (Show)

parseLine :: Parser [MarkdownInline]
parseLine = Parser $ \str -> do
    (result, rest) <- runParser (parseUntilChars "\n") str
    return ([Text $ Plain result], rest)

parseLineIndicator :: Parser MarkdownBlock
parseLineIndicator = Parser $ \str -> do
    (line, rest) <- runParser (parseUntilChars "\n" <* removePadding) str
    case line of
        "---" -> return (LineIndicator, rest)
        _ -> Nothing

parseMdHeaderInfo :: Parser MarkdownBlock
parseMdHeaderInfo = Parser $ \str -> do
    (line, rest) <- runParser (parseUntilChars "\n") str
    (key, value) <- runParser
        (parseUntilChars ":" <* parseChar ':' <* removePadding) line
    return (HeaderInfo key value, rest)

parseMdTitle :: Parser MarkdownBlock
parseMdTitle = Parser $ \str -> case str of
    ('#':'#':'#':'#':' ':xs) -> runParser (titleHandler 4) xs
    ('#':'#':'#':' ':xs) -> runParser (titleHandler 3) xs
    ('#':'#':' ':xs) -> runParser (titleHandler 2) xs
    ('#':' ':xs) -> runParser (titleHandler 1) xs
    _ -> Nothing

parseMdListNode :: Parser MarkdownBlock
parseMdListNode = Parser $ \str -> case str of
    ('-':' ':xs) -> do
        (inline, rest) <- runParser parseLine xs
        return (ListNode inline, rest)
    _ -> Nothing

titleHandler :: Int -> Parser MarkdownBlock
titleHandler nb = Parser $ \str -> do
    (inline, rest) <- runParser parseLine str
    return (Title nb inline, rest)

parseMdParagraph :: Parser MarkdownBlock
parseMdParagraph = Parser $ \str -> do
    (result, rest) <- runParser parseLine str
    return (Paragraph result, rest)
