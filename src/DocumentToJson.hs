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

bodyToJson :: Body -> JsonValue
bodyToJson (Body content) = JArray (map contentToJson content)

contentToJson :: Content -> JsonValue
contentToJson (CParagraph (Paragraph paragraph)) = JArray (map paragraphContentToJson paragraph)
contentToJson (CImage img) = imageToJson img
contentToJson (CLink link) = linkToJson link
contentToJson _ = Null

paragraphContentToJson :: ParagraphContent -> JsonValue
paragraphContentToJson (PTextFormat text) = formatToJson text
paragraphContentToJson (PImage img) = imageToJson img
paragraphContentToJson (PLink json) = linkToJson json

formatToJson :: Format -> JsonValue
formatToJson (FContent text) = JString text
formatToJson (Bold bold) = JObject [("bold", formatToJson bold)]
formatToJson (Italic italic) = JObject [("italic", formatToJson italic)]
formatToJson (Code code) = JObject [("code", formatToJson code)]

imageToJson :: Image -> JsonValue
imageToJson (Image alt url) = JObject[("image", JObject [
    ("url", JString url),
    ("alt", formatListToJson alt)])]

linkToJson :: Link -> JsonValue
linkToJson (Link alt url) = JObject[("link", JObject [
    ("url", JString url),
    ("content", formatListToJson alt)])]

formatListToJson :: FormatList -> JsonValue
formatListToJson (FormatList list) = JArray (map formatToJson list)

