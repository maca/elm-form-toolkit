module Chapters.Overview exposing (Model, Msg, chapter, init)

import Debug
import ElmBook.Actions exposing (..)
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Decode as Decode
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)
import Json.Encode
import Result
import Task


chapter : Chapter { x | overview : Model }
chapter =
    Chapter.chapter "Overview"
        |> Chapter.withStatefulComponent
            (\{ overview } ->
                view overview
                    |> Html.map
                        (updateStateWithCmdWith
                            (\msg state ->
                                update msg state.overview
                                    |> Tuple.mapFirst
                                        (\overview_ ->
                                            { state | overview = overview_ }
                                        )
                            )
                        )
            )
        |> Chapter.renderWithComponentList ""


type alias Model =
    { formFields : Field TeamFields Color
    , submitted : Bool
    , team : Maybe Team
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


init : Model
init =
    { formFields = teamFields
    , submitted = False
    , team = Nothing
    }



-- update : Msg -> Model -> ( Model, Cmd msg )


update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( formFields, result ) =
                    Field.update teamDecoder inputMsg model.formFields
            in
            ( { model
                | formFields = formFields
                , team = Result.toMaybe result
              }
            , Task.perform (ElmBook.Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString result))
            )

        FormSubmitted ->
            let
                json =
                    Decode.decode Decode.json model.formFields
                        |> Result.withDefault Json.Encode.null
                        |> Json.Encode.encode 0
            in
            ( { model
                | submitted = True
              }
            , Task.perform (ElmBook.Actions.logActionWithString "JSON")
                (Task.succeed
                    (Debug.toString json)
                )
            )


view : Model -> Html Msg
view model =
    Html.form
        [ onSubmit FormSubmitted, novalidate True ]
        [ Field.toHtml FormChanged model.formFields
        , Html.button [ onClick FormSubmitted ] [ Html.text "Submit" ]
        ]


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
