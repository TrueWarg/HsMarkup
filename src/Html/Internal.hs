module Html.Internal where

import Prelude(String, map, (.), concat, (<>), Show(..)) 

wrap :: String -> String -> String
wrap tag content = "<"<> tag <>">" <> content <> "</" <> tag <> ">"

html :: String -> Html
html = Html . wrap "html" . escape

head :: String -> Structure
head = Structure . wrap "head" . escape

title :: String -> Structure
title = Structure . wrap "title" . escape

body :: String -> Structure
body = Structure . wrap "body" . escape

p :: String -> Structure
p = Structure . wrap "p" . escape

h1 :: String -> Structure
h1 = Structure . wrap "h1" . escape

li :: String -> Structure
li = Structure . wrap "li" . escape

newtype Html = Html String
  deriving Show

newtype Structure = Structure String
  deriving Show

instance Semigroup Structure where
  (<>) s1 s2 = Structure (getStructureValue s1 <> getStructureValue s2) 

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

wrapInLi :: Structure -> Structure
wrapInLi (Structure value) = Structure (wrap "li" value)  

getStructureValue :: Structure -> String
getStructureValue (Structure value) = value

ul :: [Structure] -> Structure
ul = Structure . wrap "ul" . concat . map (wrap "li" . getStructureValue)

ol :: [Structure] -> Structure
ol = Structure . wrap "ol" . concat . map (wrap "li" . getStructureValue)

pre :: String -> Structure
pre = Structure . wrap "pre" . escape
