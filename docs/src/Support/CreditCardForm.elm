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


cardInformationParser : Parse.Parser CardFields CardInformation
cardInformationParser =
    Parse.succeed CardInformation
        |> Parse.andMap (Parse.field CardName Parse.string)
        |> Parse.andMap (Parse.field CardNumber creditCardNumberParser)
        |> Parse.andMap (Parse.field Cvc Parse.string)
        |> Parse.andMap (Parse.field ExpireMonth Parse.int)
        |> Parse.andMap (Parse.field ExpireYear Parse.int)


creditCardNumberParser : Parse.Parser id String
creditCardNumberParser =
    Parse.string
        |> Parse.andUpdate
            (\field rawInput ->
                let
                    { selectionStart } =
                        Field.toProperties field

                    cursorPosition =
                        rawInput
                            |> String.left selectionStart
                            |> cleanCardNumber
                            |> String.length

                    formattedNumber =
                        formatCardNumber rawInput

                    newCursorPosition =
                        countCharsUpTo Char.isDigit cursorPosition formattedNumber
                in
                { field =
                    field
                        |> Field.updateStringValue formattedNumber
                        |> Field.updateAttribute (Field.selectionStart newCursorPosition)
                        |> Field.updateAttribute (Field.selectionEnd newCursorPosition)
                , parseResult = Ok formattedNumber
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


countCharsUpTo : (Char -> Bool) -> Int -> String -> Int
countCharsUpTo predicate cursorPosition formattedString =
    formattedString
        |> String.toList
        |> List.foldl
            (\char ( pos, charCount ) ->
                if charCount >= cursorPosition then
                    ( pos, charCount )

                else if predicate char then
                    ( pos + 1, charCount + 1 )

                else
                    ( pos + 1, charCount )
            )
            ( 0, 0 )
        |> Tuple.first


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
