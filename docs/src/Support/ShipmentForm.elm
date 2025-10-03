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



-- TYPES


type alias Model =
    { formFields : Field ShipmentFields
    , submitted : Bool
    , result : Result (List (Error ShipmentFields)) Shipment
    }


type alias Shipment =
    { shipping : Address
    , notifiees : List Notifiee
    }


type alias Address =
    { firstName : String
    , lastName : String
    , address : String
    , address2 : String
    , postalCode : String
    , state : String
    , country : Countries.Country
    }


type alias Notifiee =
    { email : String
    , name : String
    }


type ShipmentFields
    = AddressNameGroup
    | AddressFirstName
    | AddressLastName
    | AddressStreet
    | Address2
    | LocalityGroup
    | PostalCode
    | AddressState
    | AddressCountry
    | NotifieeEmail
    | NotifieeName
    | Notifiees


type Msg
    = FormChanged (Field.Msg ShipmentFields)
    | FormSubmitted



-- INIT


init : Model
init =
    { formFields = shipmentForm
    , submitted = False
    , result = Err [ Error.CustomError Nothing "Waiting for input" ]
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( formFields, result ) =
                    Parse.parseUpdate shipmentParser inputMsg model.formFields
            in
            { formFields = formFields
            , result = result
            , submitted = False
            }

        FormSubmitted ->
            { model | submitted = True }



-- FORM DEFINITION


shipmentForm : Field ShipmentFields
shipmentForm =
    Field.group [] [ shippingInformationFields, notifieesFields ]


shippingInformationFields : Field ShipmentFields
shippingInformationFields =
    Field.group
        [ Field.label "Shipping Information" ]
        [ Field.group
            [ Field.class "address-name"
            , Field.identifier AddressNameGroup
            ]
            [ Field.text
                [ Field.label "First Name"
                , Field.required True
                , Field.identifier AddressFirstName
                , Field.name "shipping-first-name"
                ]
            , Field.text
                [ Field.label "Last Name"
                , Field.required True
                , Field.identifier AddressLastName
                , Field.name "shipping-last-name"
                ]
            ]
        , Field.text
            [ Field.label "Street Address"
            , Field.required True
            , Field.identifier AddressStreet
            , Field.name "shipping-address"
            ]
        , Field.text
            [ Field.label "Apt #"
            , Field.identifier Address2
            , Field.name "shipping-address-2"
            ]
        , Field.group
            [ Field.identifier LocalityGroup
            , Field.class "locality"
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
                , Field.name "shipping-state"
                ]
            , Field.select
                [ Field.label "Country"
                , Field.required True
                , Field.identifier AddressCountry
                , Field.name "shipping-country"
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


notifieesFields : Field ShipmentFields
notifieesFields =
    Field.repeatable
        [ Field.label "Notifiees"
        , Field.identifier Notifiees
        , Field.repeatableMin 1
        , Field.repeatableMax 5
        , Field.copies
            { addFieldsButton = "Add Notifiee"
            , removeFieldsButton = "Remove"
            }
        ]
        (Field.group
            [ Field.class "inline-fields" ]
            [ Field.text
                [ Field.label "Notifiee"
                , Field.required True
                , Field.identifier NotifieeName
                , Field.name "notifiee-name"
                ]
            , Field.email
                [ Field.label "Email"
                , Field.required True
                , Field.identifier NotifieeEmail
                , Field.name "notifiee-email"
                ]
            ]
        )
        []



-- PARSER


shipmentParser : Parse.Parser ShipmentFields Shipment
shipmentParser =
    Parse.map2 Shipment
        shipmentAddressParser
        shipmentNotifieesParser


shipmentAddressParser : Parse.Parser ShipmentFields Address
shipmentAddressParser =
    Parse.succeed Address
        |> Parse.andMap (Parse.field AddressFirstName Parse.string)
        |> Parse.andMap (Parse.field AddressLastName Parse.string)
        |> Parse.andMap (Parse.field AddressStreet Parse.string)
        |> Parse.andMap (Parse.field Address2 Parse.string)
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


shipmentNotifieesParser : Parse.Parser ShipmentFields (List Notifiee)
shipmentNotifieesParser =
    Parse.field Notifiees
        (Parse.list notifieeParser)


notifieeParser : Parse.Parser ShipmentFields Notifiee
notifieeParser =
    Parse.map2 Notifiee
        (Parse.field NotifieeEmail Parse.string)
        (Parse.field NotifieeName Parse.string)



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
        [ Html.h4 [] [ Html.text "Try the Form" ]
        , Html.form
            [ onSubmit FormSubmitted, novalidate True ]
            [ Field.toHtml FormChanged model.formFields
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
                                ("Parsed Shipment: " ++ shipment.shipping.firstName ++ " " ++ shipment.shipping.lastName)
                            ]
                        , Html.div
                            []
                            [ Html.text
                                ("Notifiees: " ++ String.fromInt (List.length shipment.notifiees))
                            ]
                        ]

                Err _ ->
                    failure
                        [ Html.text "There are some errors" ]

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



-- BROWSER.SANDBOX PROGRAM (for standalone usage)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
