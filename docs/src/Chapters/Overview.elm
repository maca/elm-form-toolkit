module Chapters.Overview exposing (Model, Msg, chapter, init)

import Debug
import Dict exposing (Dict)
import ElmBook.Actions exposing (..)
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Error as Error
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
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
    { formFields : Field TeamFields
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
    = FormChanged (Field.Msg TeamFields)
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
                formFields =
                    Field.update inputMsg model.formFields

                result =
                    Parse.parse teamDecoder formFields
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
                    Parse.parse Parse.json model.formFields
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


teamFields : Field.Field TeamFields
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
            , Field.stringOptions (colors |> List.map Tuple.first)
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


colors : List ( String, Color )
colors =
    [ ( "Red", Red )
    , ( "Green", Green )
    , ( "Blue", Blue )
    ]


teamDecoder : Parse.Parser TeamFields Team
teamDecoder =
    let
        colorsDict =
            Dict.fromList colors
    in
    Parse.map3 Team
        (Parse.field TeamName Parse.string)
        (Parse.field TeamMembers (Parse.list personDecoder))
        (Parse.field TeamColor
            (Parse.string
                |> Parse.andThen
                    (\colorStr ->
                        Dict.get colorStr colorsDict
                            |> Maybe.map Parse.succeed
                            |> Maybe.withDefault (Parse.fail "Cannot find color")
                    )
            )
        )


personDecoder : Parse.Parser TeamFields Person
personDecoder =
    Parse.map2 Person
        (Parse.field MemberName Parse.string)
        (Parse.field MemberAge Parse.int)
