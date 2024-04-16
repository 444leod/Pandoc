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
    ("header", headerToJson header)]
    -- ("header", headerToJson header),
    -- ("body", bodyToJson body)]

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
