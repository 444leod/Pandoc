{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- DocumentToJson
-}

module DocumentToJson
    ( documentToJson
) where

import Document
import Json

--- DOCUMENT TO JSON FUNCTIONS ---

{- | documentToJson
    Converts a valid document to a JsonValue
-}
documentToJson :: Document -> JsonValue
documentToJson (Document header body) = JObject [
    ("header", headerToJson header),
    ("body", bodyToJson body)]

{- | headerToJson
    Converts a valid header to a JsonValue
-}
headerToJson :: Header -> JsonValue
headerToJson (Header title (Just author) (Just date)) = JObject [
    ("title", JString title),
    ("date", JString date),
    ("author", JString author)]
headerToJson (Header title Nothing (Just date)) = JObject [
    ("title", JString title),
    ("date", JString date)]
headerToJson (Header title (Just author) Nothing) = JObject [
    ("title", JString title),
    ("author", JString author)]
headerToJson (Header title Nothing Nothing) = JObject [
    ("title", JString title)]

{- | bodyToJson
    Converts a valid body to a JsonValue
-}
bodyToJson :: Body -> JsonValue
bodyToJson (Body content) = JArray (map contentToJson content)

{- | contentToJson
    Converts a valid content to a JsonValue
-}
contentToJson :: Content -> JsonValue
contentToJson (CParagraph (Paragraph paragraph)) =
    JArray (map paragraphContentToJson paragraph)
contentToJson (CImage img) = imageToJson img
contentToJson (CLink link) = linkToJson link
contentToJson (CList list) = listToJson list
contentToJson (CCodeBlock codeblock) = codeblockToJson codeblock
contentToJson (CSection section) = sectionToJson section

{- | paragraphContentToJson
    Converts a valid paragraph content to a JsonValue
-}
paragraphContentToJson :: ParagraphContent -> JsonValue
paragraphContentToJson (PTextFormat text) = formatToJson text
paragraphContentToJson (PImage img) = imageToJson img
paragraphContentToJson (PLink json) = linkToJson json

{- | formatToJson
    Converts a valid format to a JsonValue
-}
formatToJson :: Format -> JsonValue
formatToJson (FContent text) = JString text
formatToJson (Bold bold) = JObject [("bold", formatToJson bold)]
formatToJson (Italic italic) = JObject [("italic", formatToJson italic)]
formatToJson (Code code) = JObject [("code", formatToJson code)]

{- | imageToJson
    Converts a valid image to a JsonValue
-}
imageToJson :: Image -> JsonValue
imageToJson (Image alt url) = JObject[("image", JObject [
    ("url", JString url),
    ("alt", formatListToJson alt)])]

{- | linkToJson
    Converts a valid link to a JsonValue
-}
linkToJson :: Link -> JsonValue
linkToJson (Link alt url) = JObject[("link", JObject [
    ("url", JString url),
    ("content", formatListToJson alt)])]

{- | listToJson
    Converts a valid list to a JsonValue
-}
listToJson :: List -> JsonValue
listToJson (List list) = JObject[("list", JArray (map listContentToJson list))]

{- | listContentToJson
    Converts a valid list content to a JsonValue
-}
listContentToJson :: ListContent -> JsonValue
listContentToJson (LParagraph (Paragraph paragraph)) =
    JArray (map paragraphContentToJson paragraph)
listContentToJson (SubList list) = listToJson list

{- | codeblockToJson
    Converts a valid codeblock to a JsonValue
-}
codeblockToJson :: CodeBlock -> JsonValue
codeblockToJson (CodeBlock paragraphArr) =
    JObject[("codeblock",
    JArray (map paragraphToJson paragraphArr))]

{- | paragraphToJson
    Converts a valid paragraph to a JsonValue
-}
paragraphToJson :: Paragraph -> JsonValue
paragraphToJson (Paragraph paragraph) =
    JArray (map paragraphContentToJson paragraph)

{- | sectionToJson
    Converts a valid section to a JsonValue
-}
sectionToJson :: Section -> JsonValue
sectionToJson (Section title content) = 
    JObject [("section",
    JObject [
        ("title", JString title),
        ("content", JArray (map contentToJson content))]
    )]

{- | formatListToJson
    Converts a valid format list to a JsonValue
-}
formatListToJson :: FormatList -> JsonValue
formatListToJson (FormatList list) = JArray (map formatToJson list)
