module Support.ShipmentForm exposing
    ( AddressFields(..)
    , CardFields(..)
    , RecipientFields
    , ShipmentFields(..)
    , cardFields
    , creditCardView
    , recipientsFields
    , shipmentFields
    , shipmentParser
    , shippingInformationFields
    )

import Countries
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import FormToolkit.View as View
import Html exposing (Html)
import Support.Shipment as Shipment


type ShipmentFields
    = AddressFields AddressFields
    | CardFields CardFields
    | RecipientFields RecipientFields


type AddressFields
    = AddressNameGroup
    | AddressFirstName
    | AddressLastName
    | Address
    | Address2
    | LocalityGroup
    | PostalCode
    | AddressState
    | AddressCountry


type CardFields
    = CardInfo
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
    Field.group
        []
        [ Field.map AddressFields shippingInformationFields
        , Field.map CardFields cardFields
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
            , Field.identifier Address
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


cardFields : Field CardFields
cardFields =
    Field.group
        [ Field.label "Card Information"
        , Field.identifier CardInfo
        ]
        [ Field.text
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
        , Field.group
            [ Field.class "card-params" ]
            [ Field.text
                [ Field.label "Expiration"
                , Field.required True
                , Field.identifier ExpireMonth
                , Field.name "billing-expire-month"
                , Field.placeholder "MM/YY"
                ]
            , Field.text
                [ Field.label "CVC"
                , Field.required True
                , Field.identifier Cvc
                , Field.name "billing-cvc"
                , Field.placeholder "CVC"
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
        |> Parse.andMap (field AddressFirstName Parse.string)
        |> Parse.andMap (field AddressLastName Parse.string)
        |> Parse.andMap (field Address Parse.string)
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


cardInformationParser : (CardFields -> id) -> Parse.Parser id Shipment.CardInformation
cardInformationParser toId =
    let
        field id =
            Parse.field (toId id)
    in
    Parse.succeed Shipment.CardInformation
        |> Parse.andMap (field CardName Parse.string)
        |> Parse.andMap (field CardNumber Parse.string)
        |> Parse.andMap (field Cvc Parse.string)
        |> Parse.andMap (field ExpireMonth Parse.int)
        |> Parse.andMap (field ExpireYear Parse.int)


recipientsParser : (RecipientFields -> id) -> Parse.Parser id (List Shipment.Recipient)
recipientsParser toId =
    Parse.succeed
        [ { email = ""
          , name = ""
          }
        ]


creditCardView : (Field.Msg ShipmentFields -> msg) -> Field ShipmentFields -> Html msg
creditCardView msg formFields =
    formFields
        |> Field.updateWithId (CardFields CardName)
            (Field.updateAttribute (Field.placeholder "Name on card"))
        |> Result.andThen
            (Field.updateWithId (CardFields CardNumber)
                (Field.updateAttribute (Field.placeholder "Card number"))
            )
        |> Result.map (View.fromField msg)
        |> Result.toMaybe
        |> Maybe.andThen (View.partial (CardFields CardInfo))
        |> Maybe.map View.toHtml
        |> Maybe.withDefault (Html.text "")
