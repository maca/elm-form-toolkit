module Support.ShipmentForm exposing
    ( Address
    , AddressFields(..)
    , Recipient
    , RecipientFields
    , Shipment
    , ShipmentFields(..)
    , shipmentFields
    , shipmentParser
    )

import Countries
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value


type alias Shipment =
    { shipping : Address
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


shipmentFields : Field ShipmentFields
shipmentFields =
    Field.group
        []
        [ Field.map AddressFields shippingInformationFields
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
    Parse.map2 Shipment
        shipmentAddressParser
        shipmentRecipientsParser


shipmentAddressParser : Parse.Parser ShipmentFields Address
shipmentAddressParser =
    Parse.succeed Address
        |> Parse.andMap (Parse.field (AddressFields AddressFirstName) Parse.string)
        |> Parse.andMap (Parse.field (AddressFields AddressLastName) Parse.string)
        |> Parse.andMap (Parse.field (AddressFields AddressStreet) Parse.string)
        |> Parse.andMap (Parse.field (AddressFields Address2) Parse.string)
        |> Parse.andMap (Parse.field (AddressFields PostalCode) Parse.string)
        |> Parse.andMap (Parse.field (AddressFields AddressState) Parse.string)
        |> Parse.andMap shipmentCountryParser


shipmentCountryParser : Parse.Parser ShipmentFields Countries.Country
shipmentCountryParser =
    Parse.field (AddressFields AddressCountry)
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


shipmentRecipientsParser : Parse.Parser ShipmentFields (List Recipient)
shipmentRecipientsParser =
    Parse.succeed [ { email = "", name = "" } ]
