module Support.ShipmentForm exposing
    ( Address
    , AddressFields(..)
    , Recipient
    , RecipientFields
    , Shipment
    , ShipmentFields(..)
    , parse
    , recipientsFields
    , shipmentFields
    , shipmentParser
    , shippingInformationFields
    )

import Countries
import FormToolkit.Error exposing (Error)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import FormToolkit.View as View
import Html exposing (Html)
import Support.CreditCardForm as CreditCardForm


type alias Shipment =
    { shipping : Address
    , billing : CreditCardForm.CardInformation
    , recipients : List Recipient
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


type alias Recipient =
    { email : String
    , name : String
    }


type ShipmentFields
    = AddressFields AddressFields
    | CardFields CreditCardForm.CardFields
    | RecipientFields RecipientFields


type AddressFields
    = AddressNameGroup
    | AddressFirstName
    | AddressLastName
    | AddressStreet
    | Address2
    | LocalityGroup
    | PostalCode
    | AddressState
    | AddressCountry


type RecipientFields
    = RecipientEmail
    | RecipientName


parse : Field ShipmentFields -> ( Field ShipmentFields, Result (List (Error ShipmentFields)) Shipment )
parse =
    Parse.parse shipmentParser


shipmentFields : Field ShipmentFields
shipmentFields =
    Field.group
        []
        [ Field.map AddressFields shippingInformationFields
        , Field.map CardFields CreditCardForm.cardFields
        , Field.map RecipientFields recipientsFields
        ]


shippingInformationFields : Field AddressFields
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


recipientsFields : Field RecipientFields
recipientsFields =
    Field.group
        [ Field.label "Receipt" ]
        [ Field.text
            [ Field.label "Recipient Email"
            , Field.required True
            , Field.identifier RecipientEmail
            , Field.name "recipient-email"
            ]
        , Field.text
            [ Field.label "Recipient Name"
            , Field.required True
            , Field.identifier RecipientName
            , Field.name "recipient-name"
            ]
        ]


shipmentParser : Parse.Parser ShipmentFields Shipment
shipmentParser =
    Parse.map3 Shipment
        (addressParser AddressFields)
        (CreditCardForm.cardInformationParser CardFields)
        (recipientsParser RecipientFields)


shipmentValidator : (CreditCardForm.CardFields -> id) -> Parse.Parser id ()
shipmentValidator toId =
    let
        field id =
            Parse.field (toId id)
    in
    Parse.succeed (always ())
        |> Parse.andMap
            (field CreditCardForm.CardNumber CreditCardForm.creditCardNumberParser
                |> Parse.andThen (always (Parse.succeed ()))
            )


addressParser : (AddressFields -> id) -> Parse.Parser id Address
addressParser toId =
    let
        field id =
            Parse.field (toId id)
    in
    Parse.succeed Address
        |> Parse.andMap (field AddressFirstName Parse.string)
        |> Parse.andMap (field AddressLastName Parse.string)
        |> Parse.andMap (field AddressStreet Parse.string)
        |> Parse.andMap (field Address2 Parse.string)
        |> Parse.andMap (field PostalCode Parse.string)
        |> Parse.andMap (field AddressState Parse.string)
        |> Parse.andMap (countryParser toId)


countryParser : (AddressFields -> id) -> Parse.Parser id Countries.Country
countryParser toId =
    Parse.field (toId AddressCountry)
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


recipientsParser : (RecipientFields -> id) -> Parse.Parser id (List Recipient)
recipientsParser toId =
    Parse.succeed [ { email = "", name = "" } ]
