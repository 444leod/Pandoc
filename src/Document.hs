{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Document
-}

module Document
    ( Document(..)
    , Header(..)
    , Date(..)
    , Body(..)
    , Content(..)
    , Section(..)
    , Paragraph(..)
    , ParagraphContent(..)
    , Image(..)
    , Link(..)
    , List(..)
    , ListContent(..)
    , Text(..)
    , Style(..)
) where

-- DOCUMENT

data Document = Document {
    _header :: Header,
    _body :: Body
}

-- DOCUMENT/HEADER

data Header = Header {
    _title :: String,
    _author :: String,
    _date :: String
}

-- DOCUMENT/HEADER/DATE
data Date = Date {
    _year :: Int,
    _day :: Int,
    _month :: Int
}

-- DOCUMENT/BODY

newtype Body = Body {_content :: [Content]}

-- DOCUMENT/BODY/CONTENT

data Content = 
    CSection [Section] |
    CParagraph [Paragraph] |
    CList [List] |
    CodeBlock String

-- DOCUMENT/BODY/CONTENT/SECTION

data Section = Section {
    _sectionTitle :: String,
    _sectionContent :: [Content]
}

-- DOCUMENT/BODY/CONTENT/PARAGRAPH

newtype Paragraph = Paragraph {_paragraphContent :: [ParagraphContent]}

-- DOCUMENT/BODY/CONTENT/PARAGRAPH/PARAGRAPHCONTENT

data ParagraphContent = 
    PText Text |
    PImage Image |
    PLink Link

-- DOCUMENT/BODY/CONTENT/PARAGRAPH/PARAGRAPHCONTENT/IMAGE

data Image = Image {
    _imgText :: String,
    _imgPath :: String
}

-- DOCUMENT/BODY/CONTENT/PARAGRAPH/PARAGRAPHCONTENT/LINK

data Link = Link {
    _linkText :: String,
    _linkPath :: String
}

-- DOCUMENT/BODY/CONTENT/LIST

newtype List = List {_listContent :: [ListContent]}

-- DOCUMENT/BODY/CONTENT/LIST/LISTCONTENT

data ListContent =
    ListText Text |
    SubList List

-- DOCUMENT/BODY/CONTENT/**/TEXT

data Text = Text {
    _value :: String,
    _style :: Style
}

-- DOCUMENT/BODY/CONTENT/**/TEXT/STYLE

data Style = Style {
    _bold :: Bool,
    _italic :: Bool,
    _code :: Bool
}
