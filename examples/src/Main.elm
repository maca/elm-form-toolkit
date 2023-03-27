module Main exposing (..)

import Browser
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


type alias Model =
    Form


init : Model
init =
    Form.init
        [ Input.text
            [ Input.label "First Name"
            , Input.required True
            ]
        ]



-- UPDATE


type Msg
    = FormChanged Form.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged formMsg ->
            Form.update formMsg model



-- VIEW


view : Model -> Html Msg
view form =
    div []
        [ Form.toHtml [ Form.onChange FormChanged ] form
        ]
