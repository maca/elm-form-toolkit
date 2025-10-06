module Support.CreditCardForm exposing (Model, Msg, init, update, view)

import FormToolkit.Error as Error exposing (Error)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import Html exposing (Html)
import Html.Attributes as Attr exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)



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


success : List (Html msg) -> Html msg
success =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#e8f5e8"
        , Attr.style "border-radius" "4px"
        ]


failure : List (Html msg) -> Html msg
failure =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#fde8e8"
        , Attr.style "border-radius" "4px"
        ]



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

                    formattedNumber =
                        formatMask "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}" rawInput

                    newCursorPosition =
                        calculateCursorPosition rawInput formattedNumber selectionStart
                in
                { field =
                    field
                        |> Field.updateStringValue formattedNumber
                        |> Field.updateAttribute (Field.selectionStart newCursorPosition)
                        |> Field.updateAttribute (Field.selectionEnd newCursorPosition)
                , parser = Parse.succeed formattedNumber
                }
            )


calculateCursorPosition : String -> String -> Int -> Int
calculateCursorPosition original formatted cursorPos =
    matchChars
        (String.toList (String.left cursorPos original))
        (String.toList formatted)
        0


matchChars : List Char -> List Char -> Int -> Int
matchChars source target targetPos =
    case source of
        [] ->
            targetPos

        sourceChar :: remainingSource ->
            case target of
                [] ->
                    targetPos

                targetChar :: remainingTarget ->
                    if sourceChar == targetChar then
                        matchChars remainingSource remainingTarget (targetPos + 1)

                    else
                        matchChars source remainingTarget (targetPos + 1)


type MaskToken
    = Digit -- {d}
    | NonDigit -- {D}
    | WordChar -- {w}
    | NonWordChar -- {W}
    | Literal Char -- any other character


type alias InputChar =
    ( Char, MaskToken )


formatMask : String -> String -> String
formatMask mask input =
    let
        inputChars =
            input
                |> String.toList
                |> List.map
                    (\char ->
                        ( char
                        , if Char.isDigit char then
                            Digit

                          else if Char.isAlphaNum char || char == '_' then
                            WordChar

                          else
                            NonWordChar
                        )
                    )
    in
    formatHelper (parseMask mask) inputChars []
        |> String.fromList


formatHelper : List MaskToken -> List InputChar -> List Char -> List Char
formatHelper maskList inputList acc =
    case ( maskList, inputList ) of
        ( [], _ ) ->
            List.reverse acc

        ( _, [] ) ->
            List.reverse acc

        ( maskToken :: restMask, ( char, inputToken ) :: restInput ) ->
            case maskToken of
                Literal literalChar ->
                    formatHelper restMask inputList (literalChar :: acc)

                _ ->
                    if tokensCompatible maskToken inputToken then
                        formatHelper restMask restInput (char :: acc)

                    else
                        formatHelper maskList restInput acc


parseMask : String -> List MaskToken
parseMask mask =
    parseMaskHelper (String.toList mask) []


parseMaskHelper : List Char -> List MaskToken -> List MaskToken
parseMaskHelper chars acc =
    case chars of
        [] ->
            List.reverse acc

        '{' :: 'd' :: '}' :: rest ->
            parseMaskHelper rest (Digit :: acc)

        '{' :: 'D' :: '}' :: rest ->
            parseMaskHelper rest (NonDigit :: acc)

        '{' :: 'w' :: '}' :: rest ->
            parseMaskHelper rest (WordChar :: acc)

        '{' :: 'W' :: '}' :: rest ->
            parseMaskHelper rest (NonWordChar :: acc)

        char :: rest ->
            parseMaskHelper rest (Literal char :: acc)


tokensCompatible : MaskToken -> MaskToken -> Bool
tokensCompatible maskToken inputToken =
    case ( maskToken, inputToken ) of
        ( Digit, Digit ) ->
            True

        ( NonDigit, _ ) ->
            case inputToken of
                NonDigit ->
                    True

                WordChar ->
                    True

                NonWordChar ->
                    True

                _ ->
                    False

        ( WordChar, _ ) ->
            case inputToken of
                Digit ->
                    True

                WordChar ->
                    True

                NonWordChar ->
                    True

                _ ->
                    False

        _ ->
            False
