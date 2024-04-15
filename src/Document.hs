{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Document
-}

module Document
    ( Document(..)
    , Header(..)
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
    , Format(..)
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
} deriving (Show)

-- DOCUMENT/HEADER
{- | Header
    Represents a header, with a title, an author and a date
    There is only one header per document
-}
data Header = Header {
    _title :: String,
    _author :: Maybe String,
    _date :: Maybe String
} deriving (Show)

-- DOCUMENT/BODY
{- | Body
    Represents a body, with a list of content, this is where the content is 
    There is only one body per document
-}
newtype Body = Body {_content :: [Content]} deriving (Show)

-- DOCUMENT/BODY/CONTENT
{- | Content
    Can either be a section, a paragraph, a list or a code block
-}
data Content = 
    CSection Section |
    CParagraph Paragraph |
    CList List |
    CodeBlock String deriving (Show)

-- DOCUMENT/BODY/CONTENT/SECTION
{- | Section
    Represents a section, with a title and a list of content
    A section can contain a list of content
-}
data Section = Section {
    _sectionTitle :: String,
    _sectionContent :: [Content]
} deriving (Show)

-- DOCUMENT/BODY/CONTENT/PARAGRAPH
{- | Paragraph
    Represents a paragraph, with a list of paragraph content
    Assembling this list of paragraph content will give a sensable paragraph
-}
newtype Paragraph = Paragraph {_paragraphContent :: [ParagraphContent]}
    deriving (Show)

-- DOCUMENT/BODY/CONTENT/PARAGRAPH/PARAGRAPHCONTENT
{- | ParagraphContent
    Can either be a text, an image or a link
-}
data ParagraphContent = 
    PText Text |
    PImage Image |
    PLink Link deriving (Show)

-- DOCUMENT/BODY/CONTENT/PARAGRAPH/PARAGRAPHCONTENT/IMAGE
{- | Image
    Represents an image, with a text and an URL to the image
-}
data Image = Image {
    _imgText :: String,
    _imgURL :: String
} deriving (Show)

-- DOCUMENT/BODY/CONTENT/PARAGRAPH/PARAGRAPHCONTENT/LINK
{- | Link
    Represents a link, with a text and an URL
-}
data Link = Link {
    _linkText :: String,
    _linkURL :: String
} deriving (Show)

-- DOCUMENT/BODY/CONTENT/LIST
{- | List
    Represents a list, with a list of ListContent
    (A list can contain a list)
-}
newtype List = List {_listContent :: [ListContent]} deriving (Show)

-- DOCUMENT/BODY/CONTENT/LIST/LISTCONTENT
{- | ListContent
    Can either be a text or a sublist
-}
data ListContent =
    LParagraph Paragraph |
    SubList List deriving (Show)

-- DOCUMENT/BODY/CONTENT/**/TEXT
{- | Text
    Represents a text, with a content and a format
    A text can have a format
-}
data Text = Text {
    _textContent :: String,
    _format :: Format
} deriving (Show)

-- DOCUMENT/BODY/CONTENT/**/TEXT/FORMAT
{- | Format
    Represents a format, with a bold, an italic and a code boolean
    A Text can have have multiple formats at the same time
-}
data Format = Format {
    _bold :: Bool,
    _italic :: Bool,
    _code :: Bool
} deriving (Show)
