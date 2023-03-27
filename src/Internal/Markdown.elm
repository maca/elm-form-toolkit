module Internal.Markdown exposing (toHtml)

import Html exposing (Html, a, div)
import Html.Attributes exposing (href, rel, target, title)
import Markdown.Block as Block exposing (Block(..))
import Markdown.Inline as Inline exposing (Inline(..))


toHtml : String -> Html msg
toHtml string =
    div
        []
        (Block.parse Nothing string
            |> List.map customHtmlBlock
            |> List.concat
        )


customHtmlBlock : Block b i -> List (Html msg)
customHtmlBlock block =
    Block.defaultHtml (Just customHtmlBlock) (Just customHtmlInline) block


customHtmlInline : Inline i -> Html msg
customHtmlInline inline =
    case inline of
        Link url maybeTitle inlines ->
            if String.startsWith "http://elm-lang.org" url then
                a
                    [ href url
                    , title (Maybe.withDefault "" maybeTitle)
                    ]
                    (List.map customHtmlInline inlines)

            else
                a
                    [ href url
                    , title (Maybe.withDefault "" maybeTitle)
                    , target "_blank"
                    , rel "noopener noreferrer"
                    ]
                    (List.map customHtmlInline inlines)

        _ ->
            Inline.defaultHtml (Just customHtmlInline) inline
