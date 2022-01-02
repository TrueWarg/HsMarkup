module Html.Internal where

import Prelude(String, map, (.), concat, (<>), Show(..)) 
import Numeric.Natural

newtype Html = Html String
  deriving Show

newtype Structure = Structure String
  deriving Show

type Title = String

instance Semigroup Structure where
  (<>) s1 s2 = Structure (getStructureValue s1 <> getStructureValue s2) 

instance Monoid Structure where
  mempty = empty_

html :: Title -> Structure -> Html
html title content =
  Html
    ( wrap "html"
      ( wrap "head" (wrap "title" (escape title))
        <> wrap "body" (getStructureString content)
      )
    )

head :: String -> Structure
head = Structure . wrap "head" . escape

body :: String -> Structure
body = Structure . wrap "body" . escape

p :: String -> Structure
p = Structure . wrap "p" . escape

h1 :: String -> Structure
h1 = Structure . wrap "h1" . escape

h :: Natural -> String -> Structure
h size = Structure . wrap ("h" <> show size) . escape

li :: String -> Structure
li = Structure . wrap "li" . escape

ul :: [Structure] -> Structure
ul = Structure . wrap "ul" . concat . map (wrap "li" . getStructureValue)

ol :: [Structure] -> Structure
ol = Structure . wrap "ol" . concat . map (wrap "li" . getStructureValue)

code :: String -> Structure
code = Structure . el "pre"

empty = Structure ""

wrap :: String -> String -> String
wrap tag content = "<"<> tag <>">" <> content <> "</" <> tag <> ">"

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

render :: Html -> String
render html =
  case html of
    Html str -> str
