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
xmlToDocument (XMLValue _ _ (_:_:_:_)) = Nothing
xmlToDocument (XMLValue "document" [] childrens) = do
    header <- getHeader (head childrens)
    body <- getBody (last childrens)
    return $ Document header body
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

getBody :: XMLChild -> Maybe Body
getBody (XMLNode (XMLValue "body" [] childs)) = Body <$> getBodyContent childs
getBody _ = Nothing

getBodyContent :: [XMLChild] -> Maybe [Content]
getBodyContent [] = Just []
getBodyContent (x:xs) = do
    content <- getContent x
    contents <- getBodyContent xs
    return $ content:contents

getContent :: XMLChild -> Maybe Content
getContent (XMLText text) = Just $ CTextFormat $ FContent text
getContent (XMLNode (XMLValue "paragraph" [] childs)) = do
    paragraph <- getParagraph childs
    return $ CParagraph $ Paragraph paragraph
getContent _ = Nothing

getParagraph :: [XMLChild] -> Maybe [ParagraphContent]
getParagraph [] = Just []
getParagraph (x:xs) = do
    content <- getParagraphContent x
    rest <- getParagraph xs
    return $ content:rest

getParagraphContent :: XMLChild -> Maybe ParagraphContent
getParagraphContent (XMLText text) = Just $ PTextFormat $ FContent text
getParagraphContent _ = Nothing
