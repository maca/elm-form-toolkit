module Main exposing (..)

import Browser
import FormToolkit
import FormToolkit.Decode as Decode
import FormToolkit.Input as Input
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


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
    = Title
    | Release
    | Authors
    | FirstName
    | MiddleName
    | LastName
    | Element


personFields =
    Input.group
        []
        [ Input.text
            [ Input.label "First Name"
            , Input.required True
            , Input.identifier FirstName
            ]
        , Input.text
            [ Input.label "Middle Name"
            , Input.identifier MiddleName
            ]
        , Input.text
            [ Input.label "Last Name"
            , Input.required True
            , Input.identifier LastName
            ]
        ]


init : Model
init =
    { form = recordForm }


recordForm : Input.Input Fields
recordForm =
    Input.group []
        [ Input.group []
            [ Input.text
                [ Input.label "Title"
                , Input.required True
                , Input.identifier Title
                ]
            , Input.date
                [ Input.label "Release"
                , Input.required True
                , Input.identifier Release
                ]
            ]
        , Input.repeatable [ Input.identifier Authors ] personFields []
        , Input.elementPlaceholder Element
        ]



-- UPDATE


type Msg
    = FormChanged (FormToolkit.Msg Fields)


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged formMsg ->
            let
                ( form, _ ) =
                    FormToolkit.update formMsg (Decode.succeed ()) model.form
            in
            { model | form = form }



-- VIEW


view : Model -> Html Msg
view model =
    let
        _ =
            Decode.decode recordDecoder model.form
                |> Debug.log "Author"
    in
    div
        []
        [ FormToolkit.toHtml
            [ FormToolkit.onChange FormChanged
            , FormToolkit.elementHtml Element (text "hello")
            ]
            model.form
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
