module HsMarkup.Convert where

import qualified HsMarkup.Markup as Markup
import qualified HsMarkup.Html as Html
import HsMarkup.Env (Env(..))

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
    case structure of
        (Markup.Header size text) -> Html.h size $ Html.txtContent text
        (Markup.Paragraph text) -> Html.p $ Html.txtContent text
        (Markup.UnorderedList lines) -> Html.ul $ map (Html.p . Html.txtContent) lines
        (Markup.OrderedList lines) -> Html.ol $ map (Html.p . Html.txtContent) lines
        (Markup.CodeBlock lines) -> Html.ul $ map (Html.p . Html.txtContent) lines

convert :: Env -> String -> Markup.Document -> Html.Html
convert env title doc = 
    let
        header = Html.title (markupName env <> "-" <> title) <> Html.stylesheet (stylesheetPath env)

        article = foldMap convertStructure doc
        websiteTitle = Html.h1 (Html.link "index.html" $ Html.txtContent $ markupName env)

        body = websiteTitle <> article
    in Html.html header body

