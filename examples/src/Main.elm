module Main exposing (..)

import Browser
import FormToolkit.Error as Error exposing (Error(..))
import FormToolkit.Form as Form exposing (Form)
import FormToolkit.Input as Input
import FormToolkit.Parse as Parse exposing (Parser)
import FormToolkit.Value as Value
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
    { form =
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
            ]
    }



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
            Parse.parse recordParser model.form
                |> Debug.log "Author"
    in
    div
        []
        [ Form.toHtml
            [ Form.onChange FormChanged ]
            model.form
        ]


recordParser : Parser Fields Record
recordParser =
    Parse.map2 Record
        (Parse.field Title Parse.string)
        (Parse.field Authors (Parse.list authorParser))


authorParser : Parser Fields Author
authorParser =
    Parse.succeed Author
        |> Parse.andMap (Parse.field FirstName Parse.string)
        |> Parse.andMap (Parse.field MiddleName (Parse.maybe Parse.string))
        |> Parse.andMap (Parse.field LastName Parse.string)
