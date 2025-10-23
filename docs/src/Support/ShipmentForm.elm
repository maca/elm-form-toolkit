module Support.ShipmentForm exposing (Model, Msg, init, update, view)

import Browser
import Countries
import FormToolkit.Error as Error exposing (Error)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes as Attr exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)



-- BROWSER.SANDBOX PROGRAM (for standalone usage)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- TYPES


type alias Model =
    { shipmentFields : Field ShipmentFields
    , submitted : Bool
    , result : Result (List (Error ShipmentFields)) Shipment
    }


type alias Shipment =
    { shipping : Address
    }


type alias Address =
    { firstName : String
    , lastName : String
    , address : String
    , addressNumber : Int
    , addressExtra : Maybe String
    , postalCode : String
    , state : String
    , country : Countries.Country
    }


type ShipmentFields
    = AddressNameGroup
    | AddressFirstName
    | AddressLastName
    | AddressStreet
    | AddressNumber
    | AddressExtra
    | LocalityGroup
    | PostalCode
    | AddressState
    | AddressCountry


type Msg
    = FormChanged (Field.Msg ShipmentFields)
    | FormSubmitted



-- INIT


init : Model
init =
    { shipmentFields = shipmentFieldsDefinition
    , submitted = False
    , result = Err [ Error.CustomError Nothing "Waiting for input" ]
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( shipmentFields, result ) =
                    Parse.parseUpdate shipmentParser inputMsg model.shipmentFields
            in
            { shipmentFields = shipmentFields
            , result = result
            , submitted = False
            }

        FormSubmitted ->
            { model | submitted = True }



-- FORM DEFINITION


shipmentFieldsDefinition : Field ShipmentFields
shipmentFieldsDefinition =
    Field.group
        []
        [ Field.group
            [ Field.label "Recipient"
            , Field.name "recipient"
            , Field.identifier AddressNameGroup
            , Field.class "inline-fields"
            ]
            [ Field.text
                [ Field.label "First Name"
                , Field.identifier AddressFirstName
                , Field.name "first-name"
                , Field.required True
                ]
            , Field.text
                [ Field.label "Last Name"
                , Field.identifier AddressLastName
                , Field.name "last-name"
                , Field.required True
                ]
            ]
        , Field.group
            [ Field.name "address"
            , Field.label "Address"
            ]
            [ Field.group
                [ Field.class "inline-fields" ]
                [ Field.text
                    [ Field.label "Street Name"
                    , Field.class "column column-75"
                    , Field.required True
                    , Field.identifier AddressStreet
                    , Field.name "street-name"
                    ]
                , Field.text
                    [ Field.label "Street Number"
                    , Field.identifier AddressNumber
                    , Field.required True
                    , Field.name "address-number"
                    ]
                ]
            , Field.text
                [ Field.label "Address 2"
                , Field.identifier AddressExtra
                , Field.name "address-2"
                ]
            , Field.group
                [ Field.identifier LocalityGroup
                , Field.class "locality"
                , Field.class "inline-fields"
                ]
                [ Field.text
                    [ Field.label "Postal code"
                    , Field.required True
                    , Field.identifier PostalCode
                    , Field.name "postal-code"
                    ]
                , Field.text
                    [ Field.label "State"
                    , Field.required True
                    , Field.identifier AddressState
                    , Field.name "state"
                    ]
                , Field.select
                    [ Field.label "Country"
                    , Field.required True
                    , Field.identifier AddressCountry
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
        ]



-- PARSER


shipmentParser : Parse.Parser ShipmentFields Shipment
shipmentParser =
    Parse.map Shipment
        shipmentAddressParser


shipmentAddressParser : Parse.Parser ShipmentFields Address
shipmentAddressParser =
    Parse.succeed Address
        |> Parse.andMap (Parse.field AddressFirstName Parse.string)
        |> Parse.andMap (Parse.field AddressLastName Parse.string)
        |> Parse.andMap (Parse.field AddressStreet Parse.string)
        |> Parse.andMap (Parse.field AddressNumber Parse.int)
        |> Parse.andMap (Parse.field AddressExtra (Parse.maybe Parse.string))
        |> Parse.andMap (Parse.field PostalCode Parse.string)
        |> Parse.andMap (Parse.field AddressState Parse.string)
        |> Parse.andMap shipmentCountryParser


shipmentCountryParser : Parse.Parser ShipmentFields Countries.Country
shipmentCountryParser =
    Parse.field AddressCountry
        (Parse.string
            |> Parse.andThen
                (\countryStr ->
                    case Countries.fromCode countryStr of
                        Just country ->
                            Parse.succeed country

                        Nothing ->
                            Parse.fail "Invalid country"
                )
        )



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
            , Html.button
                [ onClick FormSubmitted
                , Attr.style "margin-top" "1rem"
                ]
                [ Html.text "Submit" ]
            ]
        , if model.submitted then
            case model.result of
                Ok shipment ->
                    success
                        [ Html.div
                            []
                            [ Html.text "Form submitted successfully!" ]
                        , Html.div
                            []
                            [ Html.text
                                ("Parsed Shipment for: " ++ shipment.shipping.firstName ++ " " ++ shipment.shipping.lastName)
                            ]
                        ]

                Err errors ->
                    failure
                        [ Html.text "There are some errors:"
                        , Html.ul []
                            (errors
                                |> List.map
                                    (\error ->
                                        Html.li [] [ Html.text (Debug.toString error) ]
                                    )
                            )
                        ]

          else
            Html.text ""
        ]


success =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#e8f5e8"
        , Attr.style "border-radius" "4px"
        ]


failure =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#fde8e8"
        , Attr.style "border-radius" "4px"
        ]
