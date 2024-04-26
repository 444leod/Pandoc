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

{- | xmlToDocument function
    Convert a XML value to a Document
    Return a Document if the XML value is a valid document, Nothing otherwise
-}
xmlToDocument :: XMLValue -> Maybe Document
xmlToDocument (XMLValue _ _ (_:_:_:_)) = Nothing
xmlToDocument (XMLValue "document" [] childrens) = do
    header <- getHeader (head childrens)
    body <- getBody (last childrens)
    return $ Document header body
xmlToDocument _ = Nothing

{- | getHeader function
    Get the header from a JSON object
    Return a Header if the object is a valid header, Nothing otherwise
-}
getHeader :: XMLChild -> Maybe Header
getHeader (XMLNode (XMLValue "header" [("title", title)] childs)) =
    Just Header {
        _title = title,
        _author = getAuthor (lookupOptionalXML "author" childs),
        _date = getDate (lookupOptionalXML "date" childs)
    }
getHeader _ = Nothing

{- | getAuthor
    Get the author from a JSON object
    Return a string if the key is present and the value is a string, Nothing otherwise
-}
getAuthor :: Maybe XMLChild -> Maybe String
getAuthor (Just (XMLNode (XMLValue "author" [] [XMLText val]))) = Just val
getAuthor _ = Nothing

{- | getDate
    Get the date from a JSON object
    Return a string if the key is present and the value is a string, Nothing otherwise
-}
getDate :: Maybe XMLChild -> Maybe String
getDate (Just(XMLNode (XMLValue "date" [] [XMLText val]))) = Just val
getDate _ = Nothing

{- | lookupOptionalString function
    Get a string from a JSON object
    Return a string if the key is present and the value is a string, Nothing otherwise
-}
lookupOptionalXML :: String -> [XMLChild] -> Maybe XMLChild
lookupOptionalXML key childs = lookup key (zip (map getName childs) childs)

{- | getName function
    Get the name from a JSON object
    Return a string
-}
getName :: XMLChild -> String
getName (XMLNode (XMLValue name _ _)) = name
getName _ = ""

{- | getBody function
    Get the body from a JSON object
    Return a Body if the object is a valid body, Nothing otherwise
-}
getBody :: XMLChild -> Maybe Body
getBody (XMLNode (XMLValue "body" [] childs)) = Body <$> getBodyContent childs
getBody _ = Nothing

{- | getBodyContent function
    Get the content of the body from a JSON array
    Return a list of Content if the array is a valid body content, Nothing otherwise
-}
getBodyContent :: [XMLChild] -> Maybe [Content]
getBodyContent [] = Just []
getBodyContent (x:xs) = do
    content <- getContent x
    contents <- getBodyContent xs
    return $ content:contents

{- | getContent function
    Get a content from a JSON value
    Return a Content if the value is a valid content, Nothing otherwise
-}
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

{- | getSection function
    Get a section from a JSON value
    Return a Section if the value is a valid section, Nothing otherwise
-}
getSection :: [XMLChild] -> String -> Maybe Section
getSection childs title = do
    content <- getBodyContent childs
    Just Section {
        _sectionTitle = title,
        _sectionContent = content
    }

{- | getFormat function
    Get a format from a JSON object
    Return a Format if the object is a valid format, Nothing otherwise
-}
getFormat :: XMLChild -> Maybe Format
getFormat (XMLNode(XMLValue "bold" [] childs)) = getBold childs
getFormat (XMLNode(XMLValue "italic" [] childs)) = getItalic childs
getFormat (XMLNode(XMLValue "code" [] childs)) = getCode childs
getFormat _ = Nothing

{- | getBold function
    Get a bold format from a JSON value
    Return a Format if the value is a valid bold format, Nothing otherwise
-}
getBold :: [XMLChild] -> Maybe Format
getBold [XMLText text] = Just $ Bold $ FContent text
getBold [XMLNode(XMLValue "bold" [] childs)] = do
    res <- getFormat (head childs)
    return $ Bold res
getBold _ = Nothing

{- | getItalic function
    Get an italic format from a JSON value
    Return a Format if the value is a valid italic format, Nothing otherwise
-}
getItalic :: [XMLChild] -> Maybe Format
getItalic [XMLText text] = Just $ Italic $ FContent text
getItalic [XMLNode(XMLValue "italic" [] childs)] = do
    res <- getFormat (head childs)
    return $ Italic res
getItalic _ = Nothing

{- | getCode function
    Get a code format from a JSON value
    Return a Format if the value is a valid code format, Nothing otherwise
-}
getCode :: [XMLChild] -> Maybe Format
getCode [XMLText text] = Just $ Code $ FContent text
getCode [XMLNode(XMLValue "code" [] childs)] = do
    res <- getFormat (head childs)
    return $ Code res
getCode _ = Nothing

{- | getParagraph function
    Get a paragraph from a JSON array
    Return a list of ParagraphContent if the array is a valid paragraph, Nothing otherwise
-}
getParagraph :: [XMLChild] -> Maybe [ParagraphContent]
getParagraph [] = Just []
getParagraph (x:xs) = do
    content <- getParagraphContent x
    rest <- getParagraph xs
    return $ content:rest

{- | getParagraphContent function
    Get a paragraph content from a JSON value
    Return a ParagraphContent if the value is a valid paragraph content, Nothing otherwise
-}
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

{- | getLink function
    Get a link from a JSON value
    Return a Link if the value is a valid link, Nothing otherwise
-}
getLink :: [XMLChild] -> String -> Maybe Link
getLink node url = do
    content <- getFormatList node
    Just Link {
        _linkText = FormatList content,
        _linkURL = url
    }

{- | getImage function
    Get an image from a JSON value
    Return an Image if the value is a valid image, Nothing otherwise
-}
getImage :: [XMLChild] -> String -> Maybe Image
getImage node url = do
    content <- getFormatList node
    Just Image {
        _imgText = FormatList content,
        _imgURL = url
    }

{- | getCodeblock function
    Get a codeblock from a JSON value
    Return a list of Paragraph if the value is a valid codeblock, Nothing otherwise
-}
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

{- | getList function
    Get a list from a JSON value
    Return a list of ListContent if the value is a valid list, Nothing otherwise
-}
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

{- | getFormatList function
    Get a list of format from a JSON array
    Return a list of Format if the array is a valid format list, Nothing otherwise
-}
getFormatList :: [XMLChild] -> Maybe [Format]
getFormatList [] = Just []
getFormatList (x:xs) = do
    content <- getFormatListContent x
    rest <- getFormatList xs
    return $ content:rest

{- | getFormatListContent function
    Get a format from a JSON value
    Return a Format if the value is a valid format, Nothing otherwise
-}
getFormatListContent :: XMLChild -> Maybe Format
getFormatListContent (XMLText text) = Just $ FContent text
getFormatListContent format = getFormat format
