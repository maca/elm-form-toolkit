module Support.ShipmentForm exposing
    ( AddressFields(..)
    , CardFields(..)
    , RecipientFields
    , ShipmentFields(..)
    , cardFields
    , recipientsFields
    , shipmentFields
    , shipmentParser
    , shippingInformationFields
    )

import Countries
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Support.Shipment as Shipment


type ShipmentFields
    = AddressFields AddressFields
    | CardFields CardFields
    | RecipientFields RecipientFields


type AddressFields
    = ShippingFirstName
    | ShippingLastName
    | ShippingAddress
    | ShippingAddress2
    | ShippingState
    | ShippingCountry


type CardFields
    = CardInfo
    | CardType
    | CardName
    | CardNumber
    | Cvc
    | ExpireMonth
    | ExpireYear


type RecipientFields
    = RecipientEmail
    | RecipientName


shipmentFields : Field ShipmentFields
shipmentFields =
    Field.group []
        [ Field.map AddressFields shippingInformationFields
        , Field.map CardFields cardFields
        , Field.map RecipientFields recipientsFields
        ]


shippingInformationFields : Field AddressFields
shippingInformationFields =
    Field.group
        [ Field.label "Shipping Information" ]
        [ Field.text
            [ Field.label "First Name"
            , Field.required True
            , Field.identifier ShippingFirstName
            , Field.name "shipping-first-name"
            ]
        , Field.text
            [ Field.label "Last Name"
            , Field.required True
            , Field.identifier ShippingLastName
            , Field.name "shipping-last-name"
            ]
        , Field.text
            [ Field.label "Street Address"
            , Field.required True
            , Field.identifier ShippingAddress
            , Field.name "shipping-address"
            ]
        , Field.text
            [ Field.label "Apt #"
            , Field.identifier ShippingAddress2
            , Field.name "shipping-address-2"
            ]
        , Field.text
            [ Field.label "State"
            , Field.required True
            , Field.identifier ShippingState
            , Field.name "shipping-state"
            ]
        , Field.select
            [ Field.label "Country"
            , Field.required True
            , Field.identifier ShippingCountry
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


cardFields : Field CardFields
cardFields =
    Field.group
        [ Field.label "Card Information"
        , Field.identifier CardInfo
        ]
        [ Field.select
            [ Field.label "Card Type"
            , Field.required True
            , Field.identifier CardType
            , Field.name "billing-card-type"
            , Field.stringOptions [ "Visa", "American Express", "Discover" ]
            ]
        , Field.text
            [ Field.label "Name on Card"
            , Field.required True
            , Field.identifier CardName
            , Field.name "billing-card-name"
            ]
        , Field.text
            [ Field.label "Card Number"
            , Field.required True
            , Field.identifier CardNumber
            , Field.name "billing-card-number"
            ]
        , Field.text
            [ Field.label "CVC"
            , Field.required True
            , Field.identifier Cvc
            , Field.name "billing-cvc"
            ]
        , Field.int
            [ Field.label "Expire Month"
            , Field.required True
            , Field.identifier ExpireMonth
            , Field.name "billing-expire-month"
            ]
        , Field.int
            [ Field.label "Expire Year"
            , Field.required True
            , Field.identifier ExpireYear
            , Field.name "billing-expire-year"
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


shipmentParser : Parse.Parser ShipmentFields Shipment.Shipment
shipmentParser =
    Parse.map3 Shipment.Shipment
        (addressParser AddressFields)
        (cardInformationParser CardFields)
        (recipientsParser RecipientFields)


addressParser : (AddressFields -> id) -> Parse.Parser id Shipment.Address
addressParser toId =
    let
        field id =
            Parse.field (toId id)
    in
    Parse.succeed Shipment.Address
        |> Parse.andMap (field ShippingFirstName Parse.string)
        |> Parse.andMap (field ShippingLastName Parse.string)
        |> Parse.andMap (field ShippingAddress Parse.string)
        |> Parse.andMap (field ShippingAddress2 Parse.string)
        |> Parse.andMap (field ShippingState Parse.string)
        |> Parse.andMap (countryParser toId)


countryParser : (AddressFields -> id) -> Parse.Parser id Countries.Country
countryParser toId =
    Parse.field (toId ShippingCountry)
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


cardInformationParser : (CardFields -> id) -> Parse.Parser id Shipment.CardInformation
cardInformationParser toId =
    let
        field id =
            Parse.field (toId id)
    in
    Parse.succeed Shipment.CardInformation
        |> Parse.andMap (cardTypeParser toId)
        |> Parse.andMap (field CardName Parse.string)
        |> Parse.andMap (field CardNumber Parse.string)
        |> Parse.andMap (field Cvc Parse.string)
        |> Parse.andMap (field ExpireMonth Parse.int)
        |> Parse.andMap (field ExpireYear Parse.int)


cardTypeParser : (CardFields -> id) -> Parse.Parser id Shipment.CardType
cardTypeParser toId =
    Parse.field (toId CardType)
        (Parse.string
            |> Parse.andThen
                (\cardTypeStr ->
                    case cardTypeStr of
                        "Visa" ->
                            Parse.succeed Shipment.Visa

                        "American Express" ->
                            Parse.succeed Shipment.AmericanExpress

                        "Discover" ->
                            Parse.succeed Shipment.Discover

                        _ ->
                            Parse.fail "Invalid card type"
                )
        )


recipientsParser : (RecipientFields -> id) -> Parse.Parser id (List Shipment.Recipient)
recipientsParser toId =
    Parse.succeed
        [ { email = ""
          , name = ""
          }
        ]
