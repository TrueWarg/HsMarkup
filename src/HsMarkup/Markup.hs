module HsMarkup.Markup
  ( Document
  , Structure(..)
  , parse
  )
where

import Data.Maybe (maybeToList)

type Document = [Structure]

data Structure
  = Header Int String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Eq, Show)

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context

    ('*' : line) : rest -> 
      let
        (stars, remainginLine) = break (\ch -> ch /= '*' && ch /= ' ') line
        -- todo: use multiple break to handle multiple spaces ?
        dropSpace str = if (last str == ' ') then drop 1 str else str  
        headerSize = length (dropSpace stars) + 1
      in
        maybe id (:) context (Header headerSize (trim remainginLine) : parseLines Nothing rest)

    ('-' : ' ' : line) : rest ->
      case context of 
        Just (UnorderedList lines) ->
          parseLines (Just (UnorderedList (lines <> [trim line]))) rest
        _ -> 
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
    
    ('>' : ' ' : line) : rest ->
      case context of 
        Just (CodeBlock lines) ->
          parseLines (Just (CodeBlock (lines <> [trim line]))) rest
        _ -> 
          maybe id (:) context (parseLines (Just (CodeBlock [trim line])) rest)

    ('#' : ' ' : line) : rest ->
      case context of 
        Just (OrderedList lines) ->
          parseLines (Just (OrderedList (lines <> [line]))) rest
        _ -> 
          maybe id (:) context (parseLines (Just (OrderedList [line])) rest)
    
    currentLine : rest ->
      let 
        line = trim currentLine
        in
          case line of 
            "" -> maybe id (:) context (parseLines Nothing rest)
            _  ->
              case context of
                Just (Paragraph text) ->
                  parseLines (Just (Paragraph (unwords [text, line]))) rest
                _ -> maybe id (:) context (parseLines (Just (Paragraph line)) rest)
                
trim :: String -> String
trim = unwords . words
