{-# language QuasiQuotes #-}

module MarkupParsingSpec where

import Test.Hspec
import HsMarkup.Markup
import Text.RawString.QQ


spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
      simple
      multiple

simple :: Spec
simple = do
  describe "base" $ do
    it "empty" $ 
      shouldBe 
      (parse "") 
      []

    it "paragraph" $
      shouldBe
        (parse "Paragraph")
        [Paragraph "Paragraph"]

    it "header 2" $
      shouldBe
        (parse "** Header 2")
        [Header 2 "Header 2"]

    it "code" $
      shouldBe
        (parse "> main = putStrLn \"Kek\"")
        [CodeBlock ["main = putStrLn \"Kek\""]]


multiple :: Spec
multiple = do
    describe "multiple" $ do
        it "example1" $
          shouldBe
          (parse example1)
          example1Result

        it "example2" $
          shouldBe
          (parse example2)
          example2Result 

        it "example3" $
          shouldBe
          (parse example3)
          example3Result 

example1 :: String
example1 = [r|
Lorem blablab blabblalbalbla sactum 
Lorem blablab blabblalbalbla sactum 
Lorem blablab blabblalbalbla sactum 
bla bla blabblalbalbla

# Item 1 of a list
# Item 2 of the same list
# Item 3 of the same list
|]

example1Result :: Document
example1Result =
  [ Paragraph "Lorem blablab blabblalbalbla sactum Lorem blablab blabblalbalbla sactum Lorem blablab blabblalbalbla sactum bla bla blabblalbalbla"
  , OrderedList
    [ "Item 1 of a list"
    , "Item 2 of the same list"
    , "Item 3 of the same list"
    ]
  ]

example2 :: String
example2 = [r|
***Header 3

- Item 1 of a list
- Item 2 of the same list
- Item 3 of the same list
|]

example2Result :: Document
example2Result =
  [ Header 3 "Header 3"
  , UnorderedList
    [ "Item 1 of a list"
    , "Item 2 of the same list"
    , "Item 3 of the same list"
    ]
  ]

example3 :: String
example3 = [r|
* Header 1

Paragraph 1

Paragraph 2

> Code 1
> Code 2
|]

example3Result :: Document
example3Result =
  [ Header 1 "Header 1"
  , Paragraph "Paragraph 1"
  , Paragraph "Paragraph 2"
  , CodeBlock
    [ "Code 1"
    , "Code 2"
    ]
  ]