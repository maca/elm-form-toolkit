module Main exposing (..)

import Browser
import FormToolkit
import FormToolkit.Decode as Decode
import FormToolkit.Input as Input
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Encode


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Author =
    { firstName : String
    , middleName : Maybe String
    , lastName : String
    }


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


type alias Record =
    { title : String

    -- , releaseDate : Date
    , authors : List Author
    }


type alias Model =
    { form : Input.Input Fields }


type Fields
    = Details
    | Title
    | Release
    | Authors
    | FirstName
    | MiddleName
    | LastName


personFields =
    Input.group
        []
        [ Input.text
            [ Input.label "First Name"
            , Input.required True
            , Input.identifier FirstName
            , Input.name "first-name"
            ]
        , Input.text
            [ Input.label "Middle Name"
            , Input.identifier MiddleName
            , Input.name "middle-name"
            ]
        , Input.text
            [ Input.label "Last Name"
            , Input.required True
            , Input.identifier LastName
            , Input.name "last-name"
            ]
        ]


init : Model
init =
    { form = recordForm }


recordForm : Input.Input Fields
recordForm =
    Input.group []
        [ Input.group
            [ Input.identifier Details
            ]
            [ Input.text
                [ Input.label "Title"
                , Input.required True
                , Input.identifier Title
                , Input.name "title"
                ]
            , Input.date
                [ Input.label "Release"
                , Input.required True
                , Input.identifier Release
                , Input.name "release"
                ]
            ]
        , Input.repeatable
            [ Input.identifier Authors
            , Input.name "authors"
            , Input.repeatableMin 1
            , Input.repeatableMax 5
            ]
            personFields
            []
        ]



-- UPDATE


type Msg
    = FormChanged (FormToolkit.Msg Fields)


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged formMsg ->
            let
                ( form, decoded ) =
                    FormToolkit.update formMsg recordDecoder model.form

                _ =
                    Debug.log "author" decoded

                _ =
                    Decode.decode (Decode.succeed ()) form
                        |> Debug.log "succ"

                _ =
                    Decode.decode Decode.json form
                        |> Result.map (Json.Encode.encode 0)
                        |> Debug.log "json"
            in
            { model | form = form }



-- VIEW


view : Model -> Html Msg
view model =
    let
        inputsView =
            FormToolkit.initView FormChanged model.form
    in
    div
        []
        [ inputsView
            |> FormToolkit.viewFor Details
            |> Maybe.map FormToolkit.toHtml
            |> Maybe.withDefault (Html.text "")
        , inputsView
            |> FormToolkit.viewFor Authors
            |> Maybe.map FormToolkit.toHtml
            |> Maybe.withDefault (Html.text "")
        ]


recordDecoder : Decode.Decoder Fields Record
recordDecoder =
    Decode.map2 Record
        (Decode.field Title Decode.string)
        (Decode.field Authors (Decode.list authorDecoder))


authorDecoder : Decode.Decoder Fields Author
authorDecoder =
    Decode.succeed Author
        |> Decode.andMap (Decode.field FirstName Decode.string)
        |> Decode.andMap (Decode.field MiddleName (Decode.maybe Decode.string))
        |> Decode.andMap (Decode.field LastName Decode.string)
