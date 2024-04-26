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
getContent (XMLNode (XMLValue "list" [] childs)) = do
    list <- getList childs
    return $ CList $ List list
getContent (XMLNode (XMLValue "link" [("url", url)] childs)) = do
    res <- getLink childs url
    return $ CLink res
getContent (XMLNode (XMLValue "image" [("url", url)] childs)) = do
    res <- getImage childs url
    return $ CImage res
getContent (XMLNode (XMLValue "section" [("title", title)] childs)) = do
    res <- getSection childs title
    return $ CSection res
getContent format = do
    format' <- getFormat format
    return $ CTextFormat format'

getSection :: [XMLChild] -> String -> Maybe Section
getSection childs title = do
    content <- getBodyContent childs
    Just Section {
        _sectionTitle = title,
        _sectionContent = content
    }

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
getParagraphContent (XMLNode(XMLValue "link" [("url", url)] childs)) = do
    res <- getLink childs url
    return $ PLink res
getParagraphContent (XMLNode (XMLValue "image" [("url", url)] childs)) = do
    res <- getImage childs url
    return $ PImage res
getParagraphContent format = do
    format' <- getFormat format
    return $ PTextFormat format'

getLink :: [XMLChild] -> String -> Maybe Link
getLink node url = do
    content <- getFormatList node
    Just Link {
        _linkText = FormatList content,
        _linkURL = url
    }

getImage :: [XMLChild] -> String -> Maybe Image
getImage node url = do
    content <- getFormatList node
    Just Image {
        _imgText = FormatList content,
        _imgURL = url
    }

getCodeblock :: [XMLChild] -> Maybe [CodeBlockContent]
getCodeblock [] = Just []
getCodeblock ((XMLNode (XMLValue "paragraph" [] childs)):xs) = do
    code <- getParagraph childs
    rest <- getCodeblock xs
    return $ CodeBlockParagraph (Paragraph code):rest
getCodeblock (x:xs) =
    case getFormatListContent x of
        Just res -> do
            rest <- getCodeblock xs
            return $ CodeBlockTextFormat res:rest
        _ -> Nothing

getList :: [XMLChild] -> Maybe [ListContent]
getList [] = Just []
getList ((XMLNode (XMLValue "paragraph" [] childs)):xs) = do
    content <- getParagraph childs
    rest <- getList xs
    return $ LParagraph (Paragraph content):rest
getList ((XMLNode (XMLValue "list" [] childs)):_) = do
    val <- getList childs
    return [SubList $ List val]
getList (x:xs) = case getFormatListContent x of
    Just res -> do
        rest <- getList xs
        return $ LTextFormat res:rest
    _ -> Nothing

getFormatList :: [XMLChild] -> Maybe [Format]
getFormatList [] = Just []
getFormatList (x:xs) = do
    content <- getFormatListContent x
    rest <- getFormatList xs
    return $ content:rest

getFormatListContent :: XMLChild -> Maybe Format
getFormatListContent (XMLText text) = Just $ FContent text
getFormatListContent format = getFormat format


