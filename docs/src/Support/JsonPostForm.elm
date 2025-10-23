module Support.JsonPostForm exposing (Model, Msg, init, main, update, view)

import Browser
import Countries
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes as Attr exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode



-- BROWSER.ELEMENT PROGRAM (for standalone usage)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- TYPES


type alias Model =
    { shipmentFields : Field String
    , submitted : Bool
    , result : Maybe (Result Http.Error Encode.Value)
    }


type Msg
    = FormChanged (Field.Msg String)
    | FormSubmitted
    | FillForm
    | ClearForm
    | GotResponse (Result Http.Error Encode.Value)



-- INIT


init : Model
init =
    { shipmentFields = shipmentFields
    , submitted = False
    , result = Nothing
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormChanged fieldMsg ->
            let
                updatedField =
                    Field.update fieldMsg model.shipmentFields
            in
            ( { model
                | shipmentFields = updatedField
                , result = Nothing
              }
            , Cmd.none
            )

        FormSubmitted ->
            case Parse.parseValidate Parse.json model.shipmentFields of
                ( updatedField, Ok jsonValue ) ->
                    ( { model
                        | shipmentFields = updatedField
                        , submitted = True
                        , result = Nothing
                      }
                    , Http.post
                        { url = "https://httpbin.org/anything"
                        , body = Http.jsonBody jsonValue
                        , expect = Http.expectJson GotResponse Decode.value
                        }
                    )

                ( updatedField, Err _ ) ->
                    ( { model | shipmentFields = updatedField }, Cmd.none )

        FillForm ->
            ( { model
                | shipmentFields =
                    case Field.updateValuesFromJson sampleValues model.shipmentFields of
                        Ok fields ->
                            fields

                        Err _ ->
                            model.shipmentFields
                , result = Nothing
              }
            , Cmd.none
            )

        ClearForm ->
            ( { model
                | shipmentFields = shipmentFields
                , result = Nothing
              }
            , Cmd.none
            )

        GotResponse result ->
            ( { model
                | result = Just result
                , submitted = False
              }
            , Cmd.none
            )



-- FORM DEFINITION


sampleValuesJson : String
sampleValuesJson =
    """{
  "recipient": {
    "first-name": "José",
    "last-name": "García"
  },
  "address": {
    "street-name": "Avenida Revolución",
    "address-number": "456",
    "address-2": "Depto 3A",
    "postal-code": "03100",
    "state": "CDMX",
    "country": "156"
  },
  "credit-card": {
    "card-name": "José García",
    "card-number": "4532123456789012",
    "expire-month": "12/25",
    "cvc": "123"
  }
}"""


sampleValues : Decode.Value
sampleValues =
    case Decode.decodeString Decode.value sampleValuesJson of
        Ok value ->
            value

        Err _ ->
            Encode.null


shipmentFields : Field String
shipmentFields =
    Field.group
        []
        [ recipientFields
        , addressFields
        , creditCardFields
        ]


recipientFields : Field String
recipientFields =
    Field.group
        [ Field.label "Recipient"
        , Field.name "recipient"
        , Field.class "inline-fields"
        ]
        [ Field.text
            [ Field.label "First Name"
            , Field.name "first-name"
            , Field.required True
            ]
        , Field.text
            [ Field.label "Last Name"
            , Field.name "last-name"
            , Field.required True
            ]
        ]


addressFields : Field String
addressFields =
    Field.group
        [ Field.name "address"
        , Field.label "Address"
        ]
        [ Field.group
            [ Field.class "inline-fields" ]
            [ Field.text
                [ Field.label "Street Name"
                , Field.class "column column-75"
                , Field.required True
                , Field.name "street-name"
                ]
            , Field.int
                [ Field.label "Street Number"
                , Field.required True
                , Field.name "address-number"
                ]
            ]
        , Field.text
            [ Field.label "Address 2"
            , Field.name "address-2"
            ]
        , Field.group
            [ Field.class "locality"
            , Field.class "inline-fields"
            ]
            [ Field.text
                [ Field.label "Postal code"
                , Field.required True
                , Field.name "postal-code"
                ]
            , Field.text
                [ Field.label "State"
                , Field.required True
                , Field.name "state"
                ]
            , Field.select
                [ Field.label "Country"
                , Field.required True
                , Field.name "country"
                , Field.options
                    (Countries.all
                        |> List.map
                            (\country ->
                                ( country.name ++ " " ++ country.flag
                                , Value.string country.code
                                )
                            )
                    )
                ]
            ]
        ]


creditCardFields : Field String
creditCardFields =
    Field.group
        [ Field.label "Card Information"
        , Field.name "credit-card"
        ]
        [ Field.text
            [ Field.label "Name on Card"
            , Field.required True
            , Field.name "card-name"
            ]
        , Field.group
            [ Field.class "card-params"
            , Field.class "inline-fields"
            ]
            [ Field.text
                [ Field.label "Card Number"
                , Field.required True
                , Field.name "card-number"
                , Field.class "column-50"
                , Field.pattern "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}"
                ]
            , Field.text
                [ Field.label "Expiration"
                , Field.required True
                , Field.name "expire-month"
                , Field.placeholder "MM/YY"
                , Field.pattern "{d}{d}/{d}{d}"
                ]
            , Field.text
                [ Field.label "CVC"
                , Field.required True
                , Field.name "cvc"
                , Field.placeholder "CVC"
                , Field.pattern "{d}{d}{d}"
                ]
            ]
        ]



-- VIEW FOR DEMO COMPONENT


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "milligram"
        , Attr.style "margin-top" "20px"
        , Attr.style "padding" "20px"
        , Attr.style "border" "1px solid #d1d1d1"
        , Attr.style "border-radius" "4px"
        ]
        [ Html.form
            [ onSubmit FormSubmitted, novalidate True ]
            [ Field.toHtml FormChanged model.shipmentFields
            , Html.div
                [ Attr.style "margin-top" "1rem"
                , Attr.style "display" "flex"
                , Attr.style "gap" "1rem"
                ]
                [ Html.button
                    [ onClick FormSubmitted ]
                    [ Html.text
                        (if model.submitted then
                            "Submitting..."

                         else
                            "Submit to httpbin.org"
                        )
                    ]
                , Html.button
                    [ onClick FillForm
                    , Attr.type_ "button"
                    ]
                    [ Html.text "Fill fields from JSON" ]
                , Html.button
                    [ onClick ClearForm
                    , Attr.type_ "button"
                    ]
                    [ Html.text "Clear fields" ]
                ]
            ]
        , case model.result of
            Just (Ok responseValue) ->
                success
                    [ Html.div []
                        [ Html.text "Successfully posted to httpbin.org!" ]
                    , Html.div []
                        [ Html.text "Server response:" ]
                    , viewJson responseValue
                    ]

            Just (Err _) ->
                failure
                    [ Html.text "httpbin.org is failing while I wanted post this JSON:"
                    , Parse.parse Parse.json model.shipmentFields
                        |> Result.map viewJson
                        |> Result.withDefault (Html.text "")
                    ]

            Nothing ->
                Html.text ""
        ]


viewJson : Decode.Value -> Html msg
viewJson value =
    Html.pre
        [ Attr.style "white-space" "pre-wrap"
        , Attr.style "margin-top" "0.5rem"
        ]
        [ Html.text (Encode.encode 2 value) ]


success : List (Html msg) -> Html msg
success =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#e8f5e8"
        , Attr.style "border-radius" "4px"
        ]


failure : List (Html msg) -> Html msg
failure =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#fde8e8"
        , Attr.style "border-radius" "4px"
        ]
