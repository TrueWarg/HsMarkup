module Markup
  ( Document
  , Structure(..)
  )
where

import Numeric.Natural
import Data.Maybe

type Document = [Structure]

data Structure
  = Header Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> case context
           Nothing -> []
           Just a -> [a]
    currentLine : rest ->
      case trim currentLine of
        "" -> case context
                 Nothing -> []
                 Just a -> a: parseLines Nothing rest
        [Char] -> 
        ">" -> case context of
                  Nothing -> parseLines Maybe CodeBlock [currentLine] rest
                  Just a -> parseLines Maybe (currentLine : a) rest
        "-" -> case context of
                  Nothing -> parseLines Maybe UnorderedList [currentLine] rest
                  Just a -> parseLines Maybe (currentLine : a) rest
        "#" -> case context of
                  Nothing -> parseLines Maybe OrderedList [currentLine] rest
                  Just a -> parseLines Maybe (currentLine : a) rest
         _ -> case context of
                  Nothing -> parseLines Maybe Paragraph currentLine rest
                  Just a -> parseLines Maybe Paragraph (unlines (reverse currentParagraph)) rest
                  
         


trim :: String -> String
trim = unwords . words
