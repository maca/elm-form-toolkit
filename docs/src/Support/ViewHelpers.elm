module Support.ViewHelpers exposing (failureDiv, successDiv)

import Html exposing (Html)
import Html.Attributes as Attr


successDiv : List (Html msg) -> Html msg
successDiv =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#e8f5e8"
        , Attr.style "border-radius" "4px"
        ]


failureDiv : List (Html msg) -> Html msg
failureDiv =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#fde8e8"
        , Attr.style "border-radius" "4px"
        ]