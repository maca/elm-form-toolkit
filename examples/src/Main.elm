module Main exposing (..)

import Browser
import FormToolkit.Form as Form exposing (Form)
import FormToolkit.Input as Input
import FormToolkit.Value as Value
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Internal.Input exposing (Error(..))


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type PersonFields
    = FirstName
    | LastName


type alias Person =
    { firstName : String
    , lastName : String
    }


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
        _ =
            Form.succeed Person
                |> Form.andMap
                    (Form.getValue Value.toString FirstName model.form)
                |> Form.andMap
                    (Form.getValue Value.toString LastName model.form)
                |> Debug.log "person"
    in
    div
        []
        [ Form.toHtml
            [ Form.onChange FormChanged ]
            model.form
        ]
