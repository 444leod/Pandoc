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
{- | Document
    Represents a document, with a header and a body
    A document is created with the help of ..ToDocument functions
    A document can be converted to a certain format with the help of
        DocumentTo.. functions
-}
data Document = Document {
    _header :: Header,
    _body :: Body
}

-- DOCUMENT/HEADER
{- | Header
    Represents a header, with a title, an author and a date
    There is only one header per document
-}
data Header = Header {
    _title :: String,
    _author :: Maybe String,
    _date :: Maybe Date
}

-- DOCUMENT/HEADER/DATE
{- | Date
    Represents a date, with a year, a day and a month
-}
data Date = Date {
    _year :: Int,
    _day :: Int,
    _month :: Int
}

-- DOCUMENT/BODY
{- | Body
    Represents a body, with a list of content, this is where the content is 
    There is only one body per document
-}
newtype Body = Body {_content :: [Content]}

-- DOCUMENT/BODY/CONTENT
{- | Content
    Can either be a section, a paragraph, a list or a code block
-}
data Content = 
    CSection Section |
    CParagraph Paragraph |
    CList List |
    CodeBlock String

-- DOCUMENT/BODY/CONTENT/SECTION
{- | Section
    Represents a section, with a title and a list of content
    A section can contain a list of content
-}
data Section = Section {
    _sectionTitle :: String,
    _sectionContent :: [Content]
}

-- DOCUMENT/BODY/CONTENT/PARAGRAPH
{- | Paragraph
    Represents a paragraph, with a list of paragraph content
    Assembling this list of paragraph content will give a sensable paragraph
-}
newtype Paragraph = Paragraph {_paragraphContent :: [ParagraphContent]}

-- DOCUMENT/BODY/CONTENT/PARAGRAPH/PARAGRAPHCONTENT
{- | ParagraphContent
    Can either be a text, an image or a link
-}
data ParagraphContent = 
    PText Text |
    PImage Image |
    PLink Link

-- DOCUMENT/BODY/CONTENT/PARAGRAPH/PARAGRAPHCONTENT/IMAGE
{- | Image
    Represents an image, with a text and an URL to the image
-}
data Image = Image {
    _imgText :: String,
    _imgURL :: String
}

-- DOCUMENT/BODY/CONTENT/PARAGRAPH/PARAGRAPHCONTENT/LINK
{- | Link
    Represents a link, with a text and an URL
-}
data Link = Link {
    _linkText :: String,
    _linkURL :: String
}

-- DOCUMENT/BODY/CONTENT/LIST
{- | List
    Represents a list, with a list of ListContent
    (A list can contain a list)
-}
newtype List = List {_listContent :: [ListContent]}

-- DOCUMENT/BODY/CONTENT/LIST/LISTCONTENT
{- | ListContent
    Can either be a text or a sublist
-}
data ListContent =
    ListText Text |
    SubList List

-- DOCUMENT/BODY/CONTENT/**/TEXT
{- | Text
    Represents a text, with a value and a style
    The value is the text itself
    The style is the style of the text
-}
data Text = Text {
    _value :: String,
    _style :: Style
}

-- DOCUMENT/BODY/CONTENT/**/TEXT/STYLE
{- | Style
    Represents a style, with a bold, an italic and a code boolean
    A text can be any conbination of bold, italic and code (or none of them)
-}
data Style = Style {
    _bold :: Bool,
    _italic :: Bool,
    _code :: Bool
}
