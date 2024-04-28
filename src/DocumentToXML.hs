{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- DocumentToXML
-}

module DocumentToXML
    ( documentToXML
) where

import Document
import XML

--- DOCUMENT TO XML FUNCTIONS ---

{- | documentToXML
    Converts a valid document to a XMLValue
-}
documentToXML :: Document -> XMLValue
documentToXML (Document header body) = XMLValue "document" [] [
    headerToXML header,
    bodyToXML body]

{- | headerToXML
    Converts a valid header to a XMLChild
-}
headerToXML :: Header -> XMLChild
headerToXML (Header title (Just author) (Just date)) = XMLNode (
    XMLValue "header" [("title", title)] [
        XMLNode (XMLValue "author" [] [XMLText author]),
        XMLNode (XMLValue "date" [] [XMLText date])])
headerToXML (Header title Nothing (Just date)) = XMLNode (
    XMLValue "header" [("title", title)] [
        XMLNode (XMLValue "date" [] [XMLText date])])
headerToXML (Header title (Just author) Nothing) = XMLNode (
    XMLValue "header" [("title", title)] [
        XMLNode (XMLValue "author" [] [XMLText author])])
headerToXML (Header title Nothing Nothing) = XMLNode (
    XMLValue "header" [("title", title)] [])

{- | bodyToXML
    Converts a valid body to a XMLChild
-}
bodyToXML :: Body -> XMLChild
bodyToXML (Body content) = XMLNode (
    XMLValue "body" [] (map contentToXML content))

{- | contentToXML
    Converts a valid content to a XMLChild
-}
contentToXML :: Content -> XMLChild
contentToXML (CParagraph (Paragraph paragraph)) = XMLNode (
    XMLValue "paragraph" [] (map paragraphContentToXML paragraph))
contentToXML (CImage img) = imageToXML img
contentToXML (CLink link) = linkToXML link
contentToXML (CList list) = listToXML list
contentToXML (CCodeBlock codeblock) = codeblockToXML codeblock
contentToXML (CSection section) = sectionToXML section
contentToXML (CTextFormat text) = formatToXML text

{- | paragraphContentToXML
    Converts a valid paragraph content to a XMLChild
-}
paragraphContentToXML :: ParagraphContent -> XMLChild
paragraphContentToXML (PTextFormat text) = formatToXML text
paragraphContentToXML (PImage img) = imageToXML img
paragraphContentToXML (PLink link) = linkToXML link

{- | formatToXML
    Converts a valid format to a XMLChild
-}
formatToXML :: Format -> XMLChild
formatToXML (FContent text) = XMLText text
formatToXML (Bold bold) = XMLNode (
    XMLValue "bold" [] [formatToXML bold])
formatToXML (Italic italic) = XMLNode (
    XMLValue "italic" [] [formatToXML italic])
formatToXML (Code code) = XMLNode (
    XMLValue "code" [] [formatToXML code])

{- | imageToXML
    Converts a valid image to a XMLChild
-}
imageToXML :: Image -> XMLChild
imageToXML (Image alt url) = XMLNode (
    XMLValue "image" [("url", url)] [
        XMLNode (XMLValue "alt" [] (formatListToXML alt))
    ])

{- | listToXML
    Converts a valid list to a XMLChild
-}
formatListToXML :: FormatList -> [XMLChild]
formatListToXML (FormatList list) = map formatToXML list

{- | linkToXML
    Converts a valid link to a XMLChild
-}
linkToXML :: Link -> XMLChild
linkToXML (Link alt url) = XMLNode (
    XMLValue "link" [("url", url)] (formatListToXML alt))

{- | listContentToXML
    Converts a valid list content to a XMLChild
-}
listContentToXML :: ListContent -> XMLChild
listContentToXML (LParagraph (Paragraph paragraph)) = XMLNode (
    XMLValue "paragraph" [] (map paragraphContentToXML paragraph))
listContentToXML (LTextFormat text) = formatToXML text
listContentToXML (SubList list) = listToXML list

{- | codeblockToXML
    Converts a valid codeblock to a XMLChild
-}
listToXML :: List -> XMLChild
listToXML (List list) = XMLNode (
    XMLValue "list" [] (map listContentToXML list))

{- | codeblockToXML
    Converts a valid codeblock to a XMLChild
-}
codeblockContentToXML :: CodeBlockContent -> XMLChild
codeblockContentToXML (CodeBlockParagraph(Paragraph paragraph)) = XMLNode (
    XMLValue "paragraph" [] (map paragraphContentToXML paragraph))
codeblockContentToXML (CodeBlockTextFormat text) = formatToXML text

{- | codeblockToXML
    Converts a valid codeblock to a XMLChild
-}
codeblockToXML :: CodeBlock -> XMLChild
codeblockToXML (CodeBlock codeBlockContent) = XMLNode (
    XMLValue "codeblock" [] (map codeblockContentToXML codeBlockContent))

{- | sectionToXML
    Converts a valid section to a XMLChild
-}
sectionToXML :: Section -> XMLChild
sectionToXML (Section title content) = XMLNode (
    XMLValue "section" [("title", title)] (map contentToXML content))
