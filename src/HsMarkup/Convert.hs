module HsMarkup.Convert where

import qualified HsMarkup.Markup as Markup
import qualified HsMarkup.Html as Html

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
    case structure of
        (Markup.Header size text) -> Html.h size $ Html.txtContent text
        (Markup.Paragraph text) -> Html.p $ Html.txtContent text
        (Markup.UnorderedList lines) -> Html.ul $ map (Html.p . Html.txtContent) lines
        (Markup.OrderedList lines) -> Html.ol $ map (Html.p . Html.txtContent) lines
        (Markup.CodeBlock lines) -> Html.ul $ map (Html.p . Html.txtContent) lines

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html title . foldMap convertStructure

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
    let 
        previews =
            map( \ (path, doc) ->
                case doc of
                    Markup.Header 1 header : article ->
                        Html.h 3 (Html.link path (Html.txtContent header))
                        <> foldMap convertStructure (take 3 article)
                        <> Html.p (Html.link path (Html.txtContent "..."))
                    _ -> Html.h 3 (Html.link path (Html.txtContent path))

            )
            files
    in
        Html.html
            "Blog"
            (
                Html.h1 (Html.link "index.html" (Html.txtContent "Blog"))
                <> Html.h 2 (Html.txtContent "Posts")
                <> mconcat previews
            )
