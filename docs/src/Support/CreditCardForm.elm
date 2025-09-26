module Support.CreditCardForm exposing
    ( CardFields(..)
    , CardInformation
    , cardFields
    , cardInformationParser
    , creditCardNumberParser
    , creditCardView
    )

import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import FormToolkit.View as View
import Html exposing (Html)


type CardFields
    = CardInfo
    | CardName
    | CardNumber
    | Cvc
    | ExpireMonth
    | ExpireYear


type alias CardInformation =
    { cardName : String
    , cardNumber : String
    , cvc : String
    , expireMonth : Int
    , expireYear : Int
    }


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


cardInformationParser : (CardFields -> id) -> Parse.Parser id CardInformation
cardInformationParser toId =
    let
        field id =
            Parse.field (toId id)
    in
    Parse.succeed CardInformation
        |> Parse.andMap (field CardName Parse.string)
        |> Parse.andMap (field CardNumber creditCardNumberParser)
        |> Parse.andMap (field Cvc Parse.string)
        |> Parse.andMap (field ExpireMonth Parse.int)
        |> Parse.andMap (field ExpireYear Parse.int)


creditCardNumberParser : Parse.Parser id String
creditCardNumberParser =
    Parse.string
        |> Parse.map formatCardNumber
        |> Parse.andUpdate
            (\field number ->
                { field = Field.updateStringValue number field
                , parseResult = Ok number
                }
            )


formatCardNumber : String -> String
formatCardNumber =
    cleanCardNumber
        >> String.left 20
        >> String.toList
        >> List.indexedMap (\i c -> ( i, c ))
        >> List.foldr
            (\( i, char ) acc ->
                if modBy 4 (i + 5) == 0 then
                    char :: ' ' :: acc

                else
                    char :: acc
            )
            []
        >> String.fromList
        >> String.trim


cleanCardNumber : String -> String
cleanCardNumber =
    String.toList
        >> List.filter Char.isDigit
        >> String.fromList


creditCardView : (Field.Msg CardFields -> msg) -> Field CardFields -> Html msg
creditCardView msg formFields =
    formFields
        |> Field.updateWithId CardName
            (Field.updateAttribute (Field.placeholder "Name on card"))
        |> Result.andThen
            (Field.updateWithId CardNumber
                (Field.updateAttribute (Field.placeholder "Card number"))
            )
        |> Result.map (View.fromField msg)
        |> Result.toMaybe
        |> Maybe.andThen (View.partial CardInfo)
        |> Maybe.map View.toHtml
        |> Maybe.withDefault (Html.text "")
