{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- JsonToDocument
-}

module JsonToDocument
    ( jsonToDocument
) where

import Document
import Json

--- JSON TO DOCUMENT FUNCTIONS ---

{- | jsonToDocument function
    Convert a JSON value to a Document
    Return a Document if the JSON value is a valid document, Nothing otherwise
-}
jsonToDocument :: JsonValue -> Maybe Document
jsonToDocument (JObject (_:_:_:_)) = Nothing
jsonToDocument (JObject obj) = do
    header <- getHeader (head obj)
    body <- getBody (last obj)
    return $ Document header body
jsonToDocument _ = Nothing

{- | getHeader function
    Get the header from a JSON object
    Return a Header if the object is a valid header, Nothing otherwise
-}
getHeader :: (String, JsonValue) -> Maybe Header
getHeader ("header", JObject obj) = do
    title <- lookup "title" obj
    case title of
        JString title' -> Just Header {
            _title = title',
            _author = lookupOptionalString "author" obj,
            _date = lookupOptionalString "date" obj
        }
        _ -> Nothing
getHeader _ = Nothing

{- | getBody function
    Get the body from a JSON object
    Return a Body if the object is a valid body, Nothing otherwise
-}
getBody :: (String, JsonValue) -> Maybe Body
getBody ("body", JArray arr) = Body <$> getBodyContent arr
getBody _ = Nothing

{- | getBodyContent function
    Get the content of the body from a JSON array
    Return a list of Content if the array is a valid body content, Nothing otherwise
-}
getBodyContent :: [JsonValue] -> Maybe [Content]
getBodyContent [] = Just []
getBodyContent (x:xs) = do
    content <- getContent x
    rest <- getBodyContent xs
    return $ content : rest

{- | getContent function
    Get a content from a JSON value
    Return a Content if the value is a valid content, Nothing otherwise
-}
getContent :: JsonValue -> Maybe Content
getContent (JArray arr) = do
    paragraph <- getParagraph arr
    return $ CParagraph $ Paragraph paragraph
getContent (JObject [("codeblock", obj)]) = do
    codeblock <- getCodeblock obj
    return $ CCodeBlock $ CodeBlock codeblock
getContent (JObject [("list", obj)]) = do
    list <- getList obj
    return $ CList $ List list
getContent (JObject [("link", val)]) = do
    res <- getLink val
    return $ CLink res
getContent (JObject [("image", val)]) = do
    res <- getImage val
    return $ CImage res
getContent (JObject [("section", val)]) = do
    res <- getSection val
    return $ CSection res
getContent _ = Nothing

{- | getSection function
    Get a section from a JSON value
    Return a Section if the value is a valid section, Nothing otherwise
-}
getSection :: JsonValue -> Maybe Section
getSection (JObject (_:_:_:_)) = Nothing
getSection (JObject obj) = case (lookup "title" obj, lookup "content" obj) of
    (Just(JString title'), Just (JArray arr)) -> do
        content' <- getBodyContent arr
        return Section {_sectionTitle = title', _sectionContent = content'}
    _ -> Nothing
getSection _ = Nothing

{- | getParagraph function
    Get a paragraph from a JSON array
    Return a list of ParagraphContent if the array is a valid paragraph, Nothing otherwise
-}
getParagraph :: [JsonValue] -> Maybe [ParagraphContent]
getParagraph [] = Just []
getParagraph (x:xs) = do
    text <- getParagraphContent x
    rest' <- getParagraph xs
    return $ text : rest'

{- | getParagraphContent function
    Get a paragraph content from a JSON value
    Return a ParagraphContent if the value is a valid paragraph content, Nothing otherwise
-}
getParagraphContent :: JsonValue -> Maybe ParagraphContent
getParagraphContent (JString str) = Just $ PTextFormat $ FContent str
getParagraphContent (JObject (_:_:_)) = Nothing
getParagraphContent (JObject [("link", val)]) = do
    res <- getLink val
    return $ PLink res
getParagraphContent (JObject [("image", val)]) = do
    res <- getImage val
    return $ PImage res
getParagraphContent (JObject obj) = do
    res <- getFormat (head obj)
    return $ PTextFormat res
getParagraphContent _ = Nothing

{- | getFormat function
    Get a format from a JSON object
    Return a Format if the object is a valid format, Nothing otherwise
-}
getFormat :: (String, JsonValue) -> Maybe Format
getFormat ("bold", val) = getBold val
getFormat ("italic", val) = getItalic val
getFormat ("code", val) = getCode val
getFormat _ = Nothing

{- | getBold function
    Get a bold format from a JSON value
    Return a Format if the value is a valid bold format, Nothing otherwise
-}
getBold :: JsonValue -> Maybe Format
getBold (JString str) = Just $ Bold $ FContent str
getBold (JObject (_:_:_)) = Nothing
getBold (JObject obj) = do
    res <- getFormat (head obj)
    return $ Bold res
getBold _ = Nothing

{- | getItalic function
    Get an italic format from a JSON value
    Return a Format if the value is a valid italic format, Nothing otherwise
-}
getItalic :: JsonValue -> Maybe Format
getItalic (JString str) = Just $ Italic $ FContent str
getItalic (JObject (_:_:_)) = Nothing
getItalic (JObject obj) = do
    res <- getFormat (head obj)
    return $ Italic res
getItalic _ = Nothing

{- | getCode function
    Get a code format from a JSON value
    Return a Format if the value is a valid code format, Nothing otherwise
-}
getCode :: JsonValue -> Maybe Format
getCode (JString str) = Just $ Code $ FContent str
getCode (JObject (_:_:_)) = Nothing
getCode (JObject obj) = do
    res <- getFormat (head obj)
    return $ Code res
getCode _ = Nothing

{- | getCodeblock function
    Get a codeblock from a JSON value
    Return a list of Paragraph if the value is a valid codeblock, Nothing otherwise
-}
getCodeblock :: JsonValue -> Maybe [Paragraph]
getCodeblock (JArray []) = Just []
getCodeblock (JArray (JArray x:xs)) =  do
    code <- getParagraph x
    rest <- getCodeblock (JArray xs)
    return $ Paragraph code : rest
getCodeblock _ = Nothing

{- | getList function
    Get a list from a JSON value
    Return a list of ListContent if the value is a valid list, Nothing otherwise
-}
getList :: JsonValue -> Maybe [ListContent]
getList (JArray (JArray x:xs)) = do
    list <- getListContent x
    rest <- getList (JArray xs)
    return (list : rest)
getList (JArray(JObject [("list", obj)]:_)) = do
    val <- getList obj
    return [SubList $ List val]
getList (JArray []) = Just []
getList _ = Nothing

{- | getListContent function
    Get a list content from a JSON value
    Return a ListContent if the value is a valid list content, Nothing otherwise
-}
getListContent :: [JsonValue] -> Maybe ListContent
getListContent arr = do
    paragraph <- getParagraph arr
    return $ LParagraph $ Paragraph paragraph

{- | getLink function
    Get a link from a JSON value
    Return a Link if the value is a valid link, Nothing otherwise
-}
getLink :: JsonValue -> Maybe Link
getLink (JObject (_:_:_:_)) = Nothing
getLink (JObject obj) = case lookup "content" obj of
    Just(JArray arr) -> do
        content <- getFormatList arr
        case lookup "url" obj of
            Just (JString url') -> Just Link {
                _linkText = FormatList content,
                _linkURL = url'
            }
            _ -> Nothing
    _ -> Nothing
getLink _ = Nothing

{- | getImage function
    Get an image from a JSON value
    Return an Image if the value is a valid image, Nothing otherwise
-}
getImage :: JsonValue -> Maybe Image
getImage (JObject (_:_:_:_)) = Nothing
getImage (JObject obj) = case lookup "alt" obj of
    Just(JArray arr) -> do
        content <- getFormatList arr
        case lookup "url" obj of
            Just (JString url') -> Just Image {
                _imgText = FormatList content,
                _imgURL = url'
            }
            _ -> Nothing
    _ -> Nothing
getImage _ = Nothing

{- | getFormatList function
    Get a list of format from a JSON array
    Return a list of Format if the array is a valid format list, Nothing otherwise
-}
getFormatList :: [JsonValue] -> Maybe [Format]
getFormatList [] = Just []
getFormatList (x:xs) = do
    text <- getFormatListContent x
    rest' <- getFormatList xs
    return $ text : rest'

{- | getFormatListContent function
    Get a format from a JSON value
    Return a Format if the value is a valid format, Nothing otherwise
-}
getFormatListContent :: JsonValue -> Maybe Format
getFormatListContent (JString str) = Just $ FContent str
getFormatListContent (JObject obj) = getFormat (head obj)
getFormatListContent _ = Nothing

{- | lookupOptionalString function
    Get a string from a JSON object
    Return a string if the key is present and the value is a string, Nothing otherwise
-}
lookupOptionalString :: String -> [(String, JsonValue)] -> Maybe String
lookupOptionalString key obj = case lookup key obj of
    Just (JString str) -> Just str
    _ -> Nothing

