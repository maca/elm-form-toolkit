module Support.ShipmentForm exposing
    ( ShipmentFields(..)
    , ShippingInformationFields(..)
    , shipmentFields
    , shipmentParser
    )

import Countries
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Support.Shipment


type ShipmentFields
    = ShippingFields ShippingInformationFields
    | BillingFields BillingInformationFields
    | ReceiptFields RecipientFields


type ShippingInformationFields
    = ShippingFirstName
    | ShippingLastName
    | ShippingAddress
    | ShippingAddress2
    | ShippingState
    | ShippingCountry


type BillingInformationFields
    = BillingCardType
    | BillingCardNumber
    | BillingCvc
    | BillingExpireMonth
    | BillingExpireYear


type RecipientFields
    = RecipientEmail
    | RecipientName


shipmentFields : Field ShipmentFields
shipmentFields =
    Field.group []
        [ -- Shipping Information
          Field.group
            [ Field.label "Shipping Information" ]
            [ Field.text
                [ Field.label "First Name"
                , Field.required True
                , Field.identifier (ShippingFields ShippingFirstName)
                , Field.name "shipping-first-name"
                ]
            , Field.text
                [ Field.label "Last Name"
                , Field.required True
                , Field.identifier (ShippingFields ShippingLastName)
                , Field.name "shipping-last-name"
                ]
            , Field.text
                [ Field.label "Street Address"
                , Field.required True
                , Field.identifier (ShippingFields ShippingAddress)
                , Field.name "shipping-address"
                ]
            , Field.text
                [ Field.label "Apt #"
                , Field.identifier (ShippingFields ShippingAddress2)
                , Field.name "shipping-address-2"
                ]
            , Field.text
                [ Field.label "State"
                , Field.required True
                , Field.identifier (ShippingFields ShippingState)
                , Field.name "shipping-state"
                ]
            , Field.select
                [ Field.label "Country"
                , Field.required True
                , Field.identifier (ShippingFields ShippingCountry)
                , Field.name "shipping-country"
                , Field.options
                    (Countries.all
                        |> List.map
                            (\country ->
                                ( country.flag ++ " " ++ country.name
                                , Value.string country.code
                                )
                            )
                    )
                ]
            ]
        , -- Billing Information
          Field.group
            [ Field.label "Billing Information" ]
            [ Field.select
                [ Field.label "Card Type"
                , Field.required True
                , Field.identifier (BillingFields BillingCardType)
                , Field.name "billing-card-type"
                , Field.stringOptions [ "Visa", "American Express", "Discover" ]
                ]
            , Field.text
                [ Field.label "Card Number"
                , Field.required True
                , Field.identifier (BillingFields BillingCardNumber)
                , Field.name "billing-card-number"
                ]
            , Field.text
                [ Field.label "CVC"
                , Field.required True
                , Field.identifier (BillingFields BillingCvc)
                , Field.name "billing-cvc"
                ]
            , Field.int
                [ Field.label "Expire Month"
                , Field.required True
                , Field.identifier (BillingFields BillingExpireMonth)
                , Field.name "billing-expire-month"
                ]
            , Field.int
                [ Field.label "Expire Year"
                , Field.required True
                , Field.identifier (BillingFields BillingExpireYear)
                , Field.name "billing-expire-year"
                ]
            ]
        , -- Receipt
          Field.group
            [ Field.label "Receipt" ]
            [ Field.text
                [ Field.label "Recipient Email"
                , Field.required True
                , Field.identifier (ReceiptFields RecipientEmail)
                , Field.name "recipient-email"
                ]
            , Field.text
                [ Field.label "Recipient Name"
                , Field.required True
                , Field.identifier (ReceiptFields RecipientName)
                , Field.name "recipient-name"
                ]
            ]
        ]


shipmentParser : Parse.Parser ShipmentFields Support.Shipment.Shipment
shipmentParser =
    Parse.map3 Support.Shipment.Shipment
        addressParser
        cardInformationParser
        recipientsParser


addressParser : Parse.Parser ShipmentFields Support.Shipment.Address
addressParser =
    Parse.succeed Support.Shipment.Address
        |> Parse.andMap (Parse.field (ShippingFields ShippingFirstName) Parse.string)
        |> Parse.andMap (Parse.field (ShippingFields ShippingLastName) Parse.string)
        |> Parse.andMap (Parse.field (ShippingFields ShippingAddress) Parse.string)
        |> Parse.andMap (Parse.field (ShippingFields ShippingAddress2) Parse.string)
        |> Parse.andMap (Parse.field (ShippingFields ShippingState) Parse.string)
        |> Parse.andMap countryParser


cardInformationParser : Parse.Parser ShipmentFields Support.Shipment.CardInformation
cardInformationParser =
    Parse.map5 Support.Shipment.CardInformation
        cardTypeParser
        (Parse.field (BillingFields BillingCardNumber) Parse.string)
        (Parse.field (BillingFields BillingCvc) Parse.string)
        (Parse.field (BillingFields BillingExpireMonth) Parse.int)
        (Parse.field (BillingFields BillingExpireYear) Parse.int)


cardTypeParser : Parse.Parser ShipmentFields Support.Shipment.CardType
cardTypeParser =
    Parse.field (BillingFields BillingCardType)
        (Parse.string
            |> Parse.andThen
                (\cardTypeStr ->
                    case cardTypeStr of
                        "Visa" ->
                            Parse.succeed Support.Shipment.Visa

                        "American Express" ->
                            Parse.succeed Support.Shipment.AmericanExpress

                        "Discover" ->
                            Parse.succeed Support.Shipment.Discover

                        _ ->
                            Parse.fail "Invalid card type"
                )
        )


recipientsParser : Parse.Parser ShipmentFields (List Support.Shipment.Recipient)
recipientsParser =
    Parse.succeed
        [ { email = ""
          , name = ""
          }
        ]


countryParser : Parse.Parser ShipmentFields Countries.Country
countryParser =
    Parse.field (ShippingFields ShippingCountry)
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
