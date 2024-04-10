module Main exposing (..)

import Browser
import FormToolkit.Decode as Decode exposing (Decoder)
import FormToolkit.Error as Error exposing (Error(..))
import FormToolkit.Form as Form exposing (Form)
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
    { form : Form Fields }


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


recordForm : Form Fields
recordForm =
    Form.init
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
        , Input.element Element
        ]



-- UPDATE


type Msg
    = FormChanged Form.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged formMsg ->
            { model | form = Form.update formMsg model.form }



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
        [ Form.toHtml
            [ Form.onChange FormChanged
            , Form.elementHtml Element (text "hello")
            ]
            model.form
        ]


recordDecoder : Decoder Fields Record
recordDecoder =
    Decode.map2 Record
        (Decode.field Title Decode.string)
        (Decode.field Authors (Decode.list authorDecoder))


authorDecoder : Decoder Fields Author
authorDecoder =
    Decode.succeed Author
        |> Decode.andMap (Decode.field FirstName Decode.string)
        |> Decode.andMap (Decode.field MiddleName (Decode.maybe Decode.string))
        |> Decode.andMap (Decode.field LastName Decode.string)
