{-
-- EPITECH PROJECT, 2024
-- Pandoc
-- File description:
-- MarkdownToDocument
-}

module MarkdownToDocument (
    markdownToDocument
) where

import Markdown
import Document
import Data.Maybe

markdownToDocument :: MarkdownValue -> Maybe Document
markdownToDocument [] = Nothing
markdownToDocument (MdLineIndicator:doc) = do
    let (top, rest) = getHeaderBlocks ([], doc)
    header <- getHeader top
    body <- getBody rest
    return $ Document header body
markdownToDocument _ = Nothing

getHeader :: MarkdownValue -> Maybe Header
getHeader values
    | isNothing title = Nothing
    | otherwise = Just Header {
            _title = fromMaybe "" title,
            _author = lookupHeader "author" values,
            _date = lookupHeader "date" values
        }
    where title = lookupHeader "title" values

getHeaderBlocks :: ([MarkdownBlock], [MarkdownBlock]) -> ([MarkdownBlock], [MarkdownBlock])
getHeaderBlocks (a, []) = (a, [])
getHeaderBlocks (values, MdLineIndicator:xs) = (values, xs)
getHeaderBlocks (values, v:xs) = getHeaderBlocks (v:values, xs)

getBody :: MarkdownValue -> Maybe Body
getBody [] = Just $ Body []
getBody (x:xs) = do
    p <- buildBodyContent x
    (Body rest) <- getBody xs
    return $ Body $ p : rest

buildBodyContent :: MarkdownBlock -> Maybe Content
buildBodyContent (MdParagraph p) = do
    content <- buildParagraph p
    return (CParagraph $ Paragraph content)
buildBodyContent _ = Nothing

buildParagraph :: [MarkdownInline] -> Maybe [ParagraphContent]
buildParagraph [] = Just []
buildParagraph (x:xs) = do
    content <- buildParagraphContent x
    rest <- buildParagraph xs
    return (content : rest)

buildParagraphContent :: MarkdownInline -> Maybe ParagraphContent
buildParagraphContent (Text format) = do
    f <- buildTextFormat format
    return $ PTextFormat f
buildParagraphContent _ = Nothing

buildTextFormat :: MarkdownFormat -> Maybe Format
buildTextFormat (Plain str) = Just $ FContent str
buildTextFormat _ = Nothing

lookupHeader :: String -> [MarkdownBlock] -> Maybe String
lookupHeader _ [] = Nothing
lookupHeader key ((MdHeaderInfo k v):xs)
    | key == k = Just v
    | otherwise = lookupHeader key xs
lookupHeader _ (_:_) = Nothing
