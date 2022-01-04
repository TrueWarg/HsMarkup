module HsMarkup.Convert where

import qualified HsMarkup.Markup as Markup
import qualified HsMarkup.Html as Html

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
    case structure of
        (Markup.Header size text) -> Html.h size text
        (Markup.Paragraph text) -> Html.p text
        (Markup.UnorderedList lines) -> Html.ul $ map Html.p lines
        (Markup.OrderedList lines) -> Html.ol $ map Html.p lines
        (Markup.CodeBlock lines) -> Html.ul $ map Html.p lines

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html title . foldMap convertStructure
