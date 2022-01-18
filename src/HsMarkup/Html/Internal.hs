module HsMarkup.Html.Internal where

import Numeric.Natural

newtype Html = Html String
  deriving Show

newtype Structure = Structure String
  deriving Show

newtype Content = Content String
  deriving Show

type Title = String

newtype Header = Header String

instance Semigroup Header where
  (<>) (Header h1) (Header h2) =
    Header (h1 <> h2)

instance Monoid Header where
  mempty = Header ""

instance Semigroup Structure where
  (<>) s1 s2 = Structure (getStructureValue s1 <> getStructureValue s2) 

instance Monoid Structure where
  mempty = empty

html :: Header -> Structure -> Html
html (Header header) content =
  Html
    ( wrap "html"
      ( wrap "head" header
        <> wrap "body" (getStructureValue content)
      )
    )

head :: String -> Structure
head = Structure . wrap "head" . escape

title :: String -> Header
title = Header . wrap "title" . escape

stylesheet :: FilePath -> Header
stylesheet path =
    Header $ "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> escape path <> "\">"

meta :: String -> String -> Header
meta name content =
  Header $ "<meta name=\"" <> escape name <> "\" content=\"" <> escape content <> "\">"

body :: String -> Structure
body = Structure . wrap "body" . escape

p :: Content -> Structure
p = Structure . wrap "p" . getContentValue

h1 :: Content -> Structure
h1 = Structure . wrap "h1" . getContentValue

h :: Int -> Content -> Structure
h size = Structure . wrap ("h" <> show size) . getContentValue

ul :: [Structure] -> Structure
ul = Structure . wrap "ul" . concat . map (wrap "li" . getStructureValue)

ol :: [Structure] -> Structure
ol = Structure . wrap "ol" . concat . map (wrap "li" . getStructureValue)

code :: String -> Structure
code = Structure . wrap "pre"

empty = Structure ""

wrap :: String -> String -> String
wrap tag content = "<"<> tag <>">" <> content <> "</" <> tag <> ">"

wrapWithAttr :: String -> String -> String -> String
wrapWithAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

escape :: String -> String
escape = let 
    escapeChar c =
        case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
    
    in 
        concat . map escapeChar

getStructureValue :: Structure -> String
getStructureValue (Structure value) = value


getContentValue :: Content -> String
getContentValue (Content value) = value

txtContent :: String -> Content
txtContent = Content . escape

link :: FilePath -> Content -> Content
link path content = 
  Content $
     wrapWithAttr
         "a"
         ("href=\"" <> escape path <> "\"")
         (getContentValue content)

img :: FilePath -> Content
img path = Content $ "<img src=\"" <> escape path <> "\">"

b :: Content -> Content
b content = Content $ wrap "b" (getContentValue content)

i :: Content -> Content
i content = Content $ wrap "i" (getContentValue content)

instance Semigroup Content where
  (<>) a b = Content (getContentValue a <> getContentValue b)

instance Monoid Content where
  mempty = Content ""

render :: Html -> String
render html =
  case html of
    Html str -> str
