module Support.CreditCardForm exposing (Model, Msg, creditCardForm, init, update, view)

import Browser
import FormToolkit.Error as Error exposing (Error)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import FormToolkit.View as View
import Html exposing (Html)
import Html.Attributes as Attr exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)
import Result



-- TYPES


type alias Model =
    { formFields : Field CardFields
    , submitted : Bool
    , result : Result (List (Error CardFields)) CardInformation
    }


type Msg
    = FormChanged (Field.Msg CardFields)
    | FormSubmitted


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



-- INIT


init : Model
init =
    { formFields = creditCardForm
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
                    Parse.parseUpdate cardInformationParser inputMsg model.formFields
            in
            { formFields = formFields
            , result = result
            , submitted = False
            }

        FormSubmitted ->
            { model | submitted = True }



-- FORM DEFINITION


creditCardForm : Field CardFields
creditCardForm =
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
            [ Field.class "card-params"
            ]
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
        [ Html.h4 [] [ Html.text "Try the Credit Card Form" ]
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
                Ok cardInfo ->
                    success
                        [ Html.div
                            []
                            [ Html.text "Credit card information submitted successfully!" ]
                        , Html.div
                            []
                            [ Html.text
                                ("Card: " ++ cardInfo.cardName ++ " - " ++ cardInfo.cardNumber)
                            ]
                        ]

                Err _ ->
                    failure
                        [ Html.text "There are some errors in the form" ]

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
