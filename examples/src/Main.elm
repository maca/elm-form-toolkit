module Main exposing (main)

import Browser
import FormToolkit.Decode as Decode
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)
import Json.Encode
import Result


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { formFields : Field TeamFields Color
    , submitted : Bool
    , team : Maybe Team
    , json : Maybe Json.Encode.Value
    }


type TeamFields
    = TeamMembers
    | TeamName
    | TeamColor
    | MemberName
    | MemberAge


type Msg
    = FormChanged (Field.Msg TeamFields Color)
    | FormSubmitted


teamFields : Field.Field TeamFields Color
teamFields =
    Field.group []
        [ Field.textarea
            [ Field.label "Team Name"
            , Field.required True
            , Field.autogrow True
            , Field.identifier TeamName
            , Field.name "team-name"
            ]
        , Field.strictAutocomplete
            [ Field.label "Color"
            , Field.required True
            , Field.identifier TeamColor
            , Field.name "team-color"
            , Field.options
                [ ( "red", Value.custom Red )
                , ( "green", Value.custom Green )
                , ( "blue", Value.custom Blue )
                ]
            ]
        , Field.group
            [ Field.label "Members (max 5)" ]
            [ Field.repeatable
                [ Field.identifier TeamMembers
                , Field.repeatableMin 1
                , Field.repeatableMax 5
                , Field.name "team-members"
                ]
                (Field.group []
                    [ Field.text
                        [ Field.label "Member Name"
                        , Field.required True
                        , Field.identifier MemberName
                        , Field.name "member-name"
                        ]
                    , Field.int
                        [ Field.label "Member Age"
                        , Field.identifier MemberAge
                        , Field.name "member-age"
                        ]
                    ]
                )
                []
            ]
        ]


init : Model
init =
    { -- You want to keep the stateful input group in your model, don't worry there are
      -- no functions or weird stuff in there
      formFields = teamFields
    , submitted = False
    , team = Nothing
    , json = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( formFields, result ) =
                    -- Validates and produces result with decoder and updates with Msg
                    Field.update teamDecoder inputMsg model.formFields
            in
            { model
                | formFields = formFields
                , team = Result.toMaybe (result |> Debug.log "result")
            }

        FormSubmitted ->
            { model
                | submitted = True

                -- Uses Field.name values as keys to build a json object
                , json =
                    Decode.decode Decode.json model.formFields
                        |> Debug.log "json result"
                        |> Result.toMaybe
            }



-- VIEW


view : Model -> Html Msg
view model =
    Html.form
        [ onSubmit FormSubmitted, novalidate True ]
        [ -- Render the form
          Field.toHtml FormChanged model.formFields
        , Html.button [ onClick FormSubmitted ] [ Html.text "Submit" ]
        ]


type Color
    = Red
    | Green
    | Blue


type alias Team =
    { name : String
    , members : List Person
    , color : Color
    }


type alias Person =
    { name : String
    , age : Int
    }


teamDecoder : Decode.Decoder TeamFields Color Team
teamDecoder =
    Decode.map3 Team
        (Decode.field TeamName Decode.string)
        (Decode.field TeamMembers (Decode.list personDecoder))
        (Decode.field TeamColor Decode.customValue)


personDecoder : Decode.Decoder TeamFields Color Person
personDecoder =
    Decode.map2 Person
        (Decode.field MemberName Decode.string)
        (Decode.field MemberAge Decode.int)
