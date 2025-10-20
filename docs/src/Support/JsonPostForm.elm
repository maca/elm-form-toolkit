module Support.JsonPostForm exposing (Model, Msg, init, update, view)

import Browser
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes as Attr exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task



-- BROWSER.ELEMENT PROGRAM (for standalone usage)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- TYPES


type alias Model =
    { jsonForm : Field Never
    , submitted : Bool
    , result : Maybe (Result Http.Error Encode.Value)
    }


type Msg
    = FormChanged (Field.Msg Never)
    | FormSubmitted
    | GotResponse (Result Http.Error Encode.Value)



-- INIT


init : Model
init =
    { jsonForm = jsonForm
    , submitted = False
    , result = Nothing
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormChanged fieldMsg ->
            let
                updatedField =
                    Field.update fieldMsg model.jsonForm
            in
            ( { model
                | jsonForm = updatedField
                , result = Nothing
              }
            , Cmd.none
            )

        FormSubmitted ->
            case Parse.parse Parse.json model.jsonForm of
                Ok jsonValue ->
                    ( { model | submitted = True, result = Nothing }
                    , Http.post
                        { url = "https://httpbin.org/post"
                        , body = Http.jsonBody jsonValue
                        , expect = Http.expectJson GotResponse Decode.value
                        }
                    )

                Err err ->
                    ( model, Cmd.none )

        GotResponse result ->
            ( { model
                | result = Just result
                , submitted = False
              }
            , Cmd.none
            )



-- FORM DEFINITION


jsonForm : Field Never
jsonForm =
    Field.group []
        [ Field.text
            [ Field.label "Username"
            , Field.required True
            , Field.name "username"
            ]
        , Field.datetime
            [ Field.label "Preferred Meeting Time"
            , Field.required True
            , Field.name "meeting-time"
            ]
        , Field.int
            [ Field.label "Tolerance for Spicy Food"
            , Field.name "spicy-tolerance"
            , Field.min (Value.int 1)
            , Field.max (Value.int 10)
            ]
        ]



-- VIEW FOR DEMO COMPONENT


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "milligram"
        , Attr.style "margin-top" "20px"
        , Attr.style "padding" "20px"
        , Attr.style "border" "1px solid #d1d1d1"
        , Attr.style "border-radius" "4px"
        ]
        [ Html.form
            [ onSubmit FormSubmitted, novalidate True ]
            [ Field.toHtml FormChanged model.jsonForm
            , Html.button
                [ onClick FormSubmitted
                , Attr.style "margin-top" "1rem"
                ]
                [ Html.text
                    (if model.submitted then
                        "Submitting..."

                     else
                        "Submit to httpbin.org"
                    )
                ]
            ]
        , case model.result of
            Just (Ok responseValue) ->
                success
                    [ Html.div []
                        [ Html.text "Successfully posted to httpbin.org!" ]
                    , Html.div []
                        [ Html.text "Server response:" ]
                    , Html.pre
                        [ Attr.style "white-space" "pre-wrap"
                        , Attr.style "margin-top" "0.5rem"
                        , Attr.style "font-size" "0.8rem"
                        ]
                        [ Html.text (Encode.encode 2 responseValue) ]
                    ]

            Just (Err httpError) ->
                failure
                    [ Html.text "HTTP request failed:"
                    , Html.div []
                        [ Html.text (httpErrorToString httpError) ]
                    ]

            Nothing ->
                Html.text ""
        ]


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body


success : List (Html msg) -> Html msg
success =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#e8f5e8"
        , Attr.style "border-radius" "4px"
        ]


failure : List (Html msg) -> Html msg
failure =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#fde8e8"
        , Attr.style "border-radius" "4px"
        ]
