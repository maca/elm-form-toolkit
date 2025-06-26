module Chapters.Customization exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions exposing (updateStateWithCmdWith)
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import Html exposing (Html)
import Html.Attributes as Attr exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)
import Result
import Support.Shipment
import Support.ShipmentForm
    exposing
        ( ShipmentFields(..)
        , ShippingInformationFields(..)
        , shipmentFields
        , shipmentParser
        )
import Task


chapter : Chapter { x | customization : Model }
chapter =
    Chapter.chapter "Customization"
        |> Chapter.withStatefulComponent
            (\{ customization } ->
                view customization
                    |> Html.map
                        (updateStateWithCmdWith
                            (\msg state ->
                                update msg state.customization
                                    |> Tuple.mapFirst
                                        (\customization_ ->
                                            { state | customization = customization_ }
                                        )
                            )
                        )
            )
        |> Chapter.renderWithComponentList ""


type alias Model =
    { formFields : Field ShipmentFields
    , shipment : Maybe Support.Shipment.Shipment
    }


type Msg
    = FormChanged (Field.Msg ShipmentFields)
    | FormSubmitted


init : Model
init =
    { formFields = shipmentFields
    , shipment = Nothing
    }


update : Msg -> Model -> ( Model, Cmd (ElmBook.Msg state) )
update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                formFields =
                    Field.update inputMsg model.formFields

                result =
                    Parse.parse shipmentParser formFields
            in
            ( { model
                | formFields = formFields
                , shipment = Result.toMaybe result
              }
            , Task.perform (ElmBook.Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString result))
            )

        FormSubmitted ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.h1 []
            [ Html.text "Customization"
            ]
        , Html.div
            [ Attr.class "milligram" ]
            [ Html.form
                [ onSubmit FormSubmitted, novalidate True ]
                [ Field.toHtml FormChanged model.formFields
                , Html.button [ onClick FormSubmitted ] [ Html.text "Submit" ]
                ]
            ]
        ]
