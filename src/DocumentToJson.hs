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

documentToJson :: Document -> JsonValue
documentToJson (Document header body) = JObject [
    ("header", headerToJson header),
    ("body", bodyToJson body)]

headerToJson :: Header -> JsonValue
headerToJson (Header title (Just author) (Just date)) = JObject [
    ("title", JString title),
    ("author", JString author),
    ("date", JString date)]
headerToJson (Header title Nothing (Just date)) = JObject [
    ("title", JString title),
    ("date", JString date)]
headerToJson (Header title (Just author) Nothing) = JObject [
    ("title", JString title),
    ("author", JString author)]
headerToJson (Header title Nothing Nothing) = JObject [
    ("title", JString title)]

bodyToJson :: Body -> JsonValue
bodyToJson (Body content) = JArray (map contentToJson content)

contentToJson :: Content -> JsonValue
contentToJson (CParagraph (Paragraph paragraph)) = JArray (map paragraphContentToJson paragraph)
contentToJson _ = Null

paragraphContentToJson :: ParagraphContent -> JsonValue
paragraphContentToJson (PTextFormat text) = formatToJson text
paragraphContentToJson _ = Null

formatToJson :: Format -> JsonValue
formatToJson (FContent text) = JString text
formatToJson (Bold bold) = JObject [("bold", formatToJson bold)]
formatToJson (Italic italic) = JObject [("italic", formatToJson italic)]
formatToJson (Code code) = JObject [("code", formatToJson code)]
