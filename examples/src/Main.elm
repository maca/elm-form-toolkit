module Main exposing (..)

import Browser
import FormToolkit.Error as Error exposing (Error(..))
import FormToolkit.Form as Form exposing (Form)
import FormToolkit.Input as Input
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


type PersonFields
    = FirstName
    | MiddleName
    | LastName


type alias Person =
    { firstName : String
    , middleName : Maybe String
    , lastName : String
    }


type Name
    = Name String


type alias Model =
    { form : Form PersonFields }


init : Model
init =
    { form =
        Form.init
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
        -- _ =
        --     Form.map Name (Form.get FirstName Value.toString model.form)
        --         |> Debug.log "person"
        _ =
            Form.succeed Person
                |> Form.andMap (Form.get FirstName Value.toString model.form)

        -- |> Form.andMap
        --     (Form.getMaybe MiddleName Value.toString model.form)
        -- |> Form.andMap (Form.get LastName Value.toString model.form)
        -- |> Debug.log "person"
    in
    div
        []
        [ Form.toHtml
            [ Form.onChange FormChanged ]
            model.form
        ]
