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

import Debug.Trace
import ParserLib

type MarkdownValue = [MarkdownBlock]

data MarkdownBlock =
    MdParagraph [MarkdownInline] | -- a line of text
    MdHeaderInfo String String | -- a line of text
    MdListNode [MarkdownInline] | -- a node of a list
    MdTitle Int [MarkdownInline] | -- a header (the number is for the importance [1-4])
    MdCodeBlock [String] | -- a chunk of code on multiple lines
    MdLineIndicator -- an horizontal line
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

parseMarkdownValue :: Parser MarkdownValue
parseMarkdownValue = Parser $ \str -> do
    (_, str') <- runParser removePadding str
    -- trace str' (return 0)
    case str' of
        "" -> return ([], "")
        _ -> do
            (block, rest1) <- runParser (parseMdHeaderInfo
                <|> parseLineIndicator
                <|> parseMdListNode
                <|> parseMdTitle
                <|> parseMdParagraph) str'
            (xs, rest2) <- runParser parseMarkdownValue rest1
            return (block:xs, rest2)

parseLine :: Parser [MarkdownInline]
parseLine = Parser $ \str -> do
    (result, rest) <- runParser (parseUntilChars "\n") str
    return ([Text $ Plain result], rest)

parseLineIndicator :: Parser MarkdownBlock
parseLineIndicator = Parser $ \str -> do
    (line, rest) <- runParser (parseUntilChars "\n" <* removePadding) str
    case line of
        "---" -> return (MdLineIndicator, rest)
        _ -> Nothing

parseMdHeaderInfo :: Parser MarkdownBlock
parseMdHeaderInfo = Parser $ \str -> do
    (line, rest) <- runParser (parseUntilChars "\n") str
    (key, value) <- runParser
        (parseUntilChars ":" <* parseChar ':' <* removePadding) line
    return (MdHeaderInfo key value, rest)

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
        return (MdListNode inline, rest)
    _ -> Nothing

titleHandler :: Int -> Parser MarkdownBlock
titleHandler nb = Parser $ \str -> do
    (inline, rest) <- runParser parseLine str
    return (MdTitle nb inline, rest)

parseMdParagraph :: Parser MarkdownBlock
parseMdParagraph = Parser $ \str -> do
    (result, rest) <- runParser parseLine str
    return (MdParagraph result, rest)
