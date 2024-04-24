{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- XMLToDocument
-}

module XMLToDocument
    ( xmlToDocument
) where

import Document
import XML
import Debug.Trace

xmlToDocument :: XMLValue -> Maybe Document
xmlToDocument (XMLValue "document" [] childrens) = do
    header <- trace (show childrens) (getHeader (head childrens))
    body <- Just (Body [])
    return $ Document header body
xmlToDocument (XMLValue _ _ (_:_:_:_)) = Nothing
xmlToDocument _ = Nothing

getHeader :: XMLChild -> Maybe Header
getHeader (XMLNode test) = Just Header {
        _title = "title",
        _author = Just "author",
        _date = Just "date"
    }
getHeader _ = Nothing