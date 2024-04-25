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
    header <- trace (show (head childrens)) (getHeader (head childrens))
    body <- Just (Body [])
    return $ Document header body
xmlToDocument (XMLValue _ _ (_:_:_:_)) = Nothing
xmlToDocument _ = Nothing

getHeader :: XMLChild -> Maybe Header
getHeader (XMLNode (XMLValue "header" [("title", title)] childs)) =
    Just Header {
        _title = title,
        _author = getAuthor (lookupOptionalXML "author" childs),
        _date = getDate (lookupOptionalXML "date" childs)
    }
getHeader _ = Nothing

getAuthor :: Maybe XMLChild -> Maybe String
getAuthor (Just (XMLNode (XMLValue "author" [] [XMLText val]))) = Just val
getAuthor _ = Nothing

getDate :: Maybe XMLChild -> Maybe String
getDate (Just(XMLNode (XMLValue "date" [] [XMLText val]))) = Just val
getDate _ = Nothing

lookupOptionalXML :: String -> [XMLChild] -> Maybe XMLChild
lookupOptionalXML key childs = lookup key (zip (map getName childs) childs)

getName :: XMLChild -> String
getName (XMLNode (XMLValue name _ _)) = name
getName _ = ""
