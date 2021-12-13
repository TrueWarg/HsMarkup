module Main where

import Lib, Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html
    "My title"
    ( append
      (h1 "Header")
      ( append
        (p "Paragraph #1")
        (p "Paragraph #2")
      )
    )
