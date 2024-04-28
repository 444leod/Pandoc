{-
-- EPITECH PROJECT, 2024
-- Pandoc
-- File description:
-- Markdown
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Markdown (
    MarkdownValue,
    MarkdownBlock(..),
    MarkdownInline(..),
    MarkdownFormat(..),
    parseMarkdownValue,
) where

import ParserLib

{- | MarkdownValue
    Represents an entire Markdown document as parsed.
    It is equivalent to a list of `MarkdownBlock`
-}
type MarkdownValue = [MarkdownBlock]

{- | MarkdownBlock
    Any Markdown element that required taking an entire line to exist.
    For example, a list node (- element) needs to be on a separate line.
-}
data MarkdownBlock =
    MdParagraph [MarkdownInline] | -- a line of text
    MdHeaderInfo String String | -- a line of text
    MdListNode [MarkdownInline] | -- a node of a list
    MdTitle Int [MarkdownInline] | -- a header (the number is for the importance [1-4])
    MdCodeBlock [String] | -- a chunk of code on multiple lines
    MdLineIndicator -- an horizontal line
    deriving (Show)

{- | MarkdownFormat
    Represents a chunk of text with modifiers.
    Uses recursive building so a bold attirbute can hold a italic one f.e.
-}
data MarkdownFormat =
    Plain String | -- a word or chunck of words
    Bold MarkdownInline | -- adds a bold attribute to
    Italic MarkdownInline | -- adds italic attribute to chunk
    Code MarkdownInline
    deriving (Show)

{- | MarkdownInline
    Any Markdown element that can be stringed in the same line.
    For exmample, chunks of texts, links, and images, can all be put multiple
    times on a singular line.
-}
data MarkdownInline =
    Text MarkdownFormat |
    Link MarkdownFormat String |
    Image MarkdownFormat String
    deriving (Show)

{- | parseMarkdownValue
    Parses a list of many markdown blocks from a string.
-}
parseMarkdownValue :: Parser MarkdownValue
parseMarkdownValue = Parser $ \str -> do
    (_, str') <- runParser removePadding str
    case str' of
        "" -> return ([], "")
        _ -> runParser parseMarkdownBlock str'

{- | parseMarkdownBlock
    Parses the next block and the tail blocks from a string.
-}
parseMarkdownBlock :: Parser MarkdownValue
parseMarkdownBlock = Parser $ \str -> do
    (block, rest1) <- runParser (parseMdHeaderInfo
        <|> parseLineIndicator
        <|> parseMdListNode
        <|> parseMdTitle
        <|> parseMdParagraph) str
    (xs, rest2) <- runParser parseMarkdownValue rest1
    return (block:xs, rest2)

{- | parseLine
    Parses a (the rest of a) line of text from a string.
-}
parseLine :: Parser [MarkdownInline]
parseLine = Parser $ \str -> do
    (result, rest) <- runParser (parseUntilChars "\n") str
    return ([Text $ Plain result], rest)

{- | parseLine
    Parses a horizontal separation (---) from a string.
-}
parseLineIndicator :: Parser MarkdownBlock
parseLineIndicator = Parser $ \str -> do
    (line, rest) <- runParser (parseUntilChars "\n" <* removePadding) str
    case line of
        "---" -> return (MdLineIndicator, rest)
        _ -> Nothing

{- | parseMdHeaderInfo
    Parses a header information (key: value) from a string.
-}
parseMdHeaderInfo :: Parser MarkdownBlock
parseMdHeaderInfo = Parser $ \str -> do
    (line, rest) <- runParser (parseUntilChars "\n") str
    (key, value) <- runParser
        (parseUntilChars ":" <* parseChar ':' <* removePadding) line
    return (MdHeaderInfo key value, rest)

{- | parseMdHeaderInfo
    Parses a markdown style header title (# Title) from a string.
-}
parseMdTitle :: Parser MarkdownBlock
parseMdTitle = Parser $ \str -> case str of
    ('#':'#':'#':'#':' ':xs) -> runParser (titleHandler 4) xs
    ('#':'#':'#':' ':xs) -> runParser (titleHandler 3) xs
    ('#':'#':' ':xs) -> runParser (titleHandler 2) xs
    ('#':' ':xs) -> runParser (titleHandler 1) xs
    _ -> Nothing

{- | titleHandler
    Creates a MarkdownBlock title from a string.
    Only used in `parseMdTitle`.
-}
titleHandler :: Int -> Parser MarkdownBlock
titleHandler nb = Parser $ \str -> do
    (inline, rest) <- runParser parseLine str
    return (MdTitle nb inline, rest)


{- | parseMdHeaderInfo
    Parses a list element (- element ) from a string.
-}
parseMdListNode :: Parser MarkdownBlock
parseMdListNode = Parser $ \str -> case str of
    ('-':' ':xs) -> do
        (inline, rest) <- runParser parseLine xs
        return (MdListNode inline, rest)
    _ -> Nothing

{- | parseMdParagraph
    Parses a complete line of text as paragraph.
-}
parseMdParagraph :: Parser MarkdownBlock
parseMdParagraph = Parser $ \str -> do
    (result, rest) <- runParser parseLine str
    return (MdParagraph result, rest)
