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
getContent (XMLNode (XMLValue "codeblock" [] childs)) = do
    codeblock <- getCodeblock childs
    return $ CCodeBlock $ CodeBlock codeblock
getContent format = do
    format' <- getFormat format
    return $ CTextFormat format'

getFormat :: XMLChild -> Maybe Format
getFormat (XMLNode(XMLValue "bold" [] childs)) = getBold childs
getFormat (XMLNode(XMLValue "italic" [] childs)) = getItalic childs
getFormat (XMLNode(XMLValue "code" [] childs)) = getCode childs
getFormat _ = Nothing

getBold :: [XMLChild] -> Maybe Format
getBold [XMLText text] = Just $ Bold $ FContent text
getBold [XMLNode(XMLValue "bold" [] childs)] = do
    res <- getFormat (head childs)
    return $ Bold res
getBold _ = Nothing

getItalic :: [XMLChild] -> Maybe Format
getItalic [XMLText text] = Just $ Italic $ FContent text
getItalic [XMLNode(XMLValue "italic" [] childs)] = do
    res <- getFormat (head childs)
    return $ Italic res
getItalic _ = Nothing

getCode :: [XMLChild] -> Maybe Format
getCode [XMLText text] = Just $ Code $ FContent text
getCode [XMLNode(XMLValue "code" [] childs)] = do
    res <- getFormat (head childs)
    return $ Code res
getCode _ = Nothing

getParagraph :: [XMLChild] -> Maybe [ParagraphContent]
getParagraph [] = Just []
getParagraph (x:xs) = do
    content <- getParagraphContent x
    rest <- getParagraph xs
    return $ content:rest

getParagraphContent :: XMLChild -> Maybe ParagraphContent
getParagraphContent (XMLText text) = Just $ PTextFormat $ FContent text
getParagraphContent format = do
    format' <- getFormat format
    return $ PTextFormat format'

getCodeblock :: [XMLChild] -> Maybe [CodeBlockContent]
getCodeblock [] = Just []
getCodeblock ((XMLNode (XMLValue "paragraph" [] childs)):xs) = do
    code <- getParagraph childs
    rest <- getCodeblock xs
    return $  CodeBlockParagraph (Paragraph code):rest
getCodeblock (x:xs) =
    case getFormatListContent x of
        Just res -> do
            rest <- getCodeblock xs
            return $ CodeBlockTextFormat res:rest
        _ -> Nothing

getFormatListContent :: XMLChild -> Maybe Format
getFormatListContent (XMLText text) = Just $ FContent text
getFormatListContent format = getFormat format
