module Support.CreditCardForm exposing (Model, Msg, init, update, view)

import Dict
import FormToolkit.Error as Error exposing (Error)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import Html exposing (Html)
import Html.Attributes as Attr exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)
import Time exposing (Month(..))



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
    | Expiration


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


update : Time.Posix -> Msg -> Model -> Model
update now msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( formFields, result ) =
                    model.formFields
                        |> Parse.parseUpdate (cardInformationParser now) inputMsg
            in
            { model
                | formFields = formFields
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
                , Field.identifier Expiration
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


cardInformationParser : Time.Posix -> Parse.Parser CardFields CardInformation
cardInformationParser now =
    Parse.succeed
        (\name number cvv ( month, year ) ->
            CardInformation name number cvv month year
        )
        |> Parse.andMap (Parse.field CardName Parse.string)
        |> Parse.andMap
            (Parse.field CardNumber
                (Parse.formattedString
                    "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}"
                )
            )
        |> Parse.andMap (Parse.field Cvc Parse.string)
        |> Parse.andMap
            (Parse.field Expiration
                (Parse.formattedString "{d}{d}/{d}{d}"
                    |> Parse.andThen
                        (\exp ->
                            case
                                String.split "/" exp
                                    |> List.filterMap String.toInt
                            of
                                month :: year :: [] ->
                                    if month > 12 then
                                        Parse.fail "The date is invalid"

                                    else
                                        let
                                            current =
                                                (Time.toYear Time.utc now * 100)
                                                    + monthToInt (Time.toMonth Time.utc now)

                                            provided =
                                                ((year + 2000) * 100) + month
                                        in
                                        if provided <= current then
                                            Parse.fail "The card has already expired"

                                        else
                                            Parse.succeed ( month, year )

                                _ ->
                                    Parse.fail "The date is invalid"
                        )
                )
            )


months : List Month
months =
    [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]


monthToInt : Month -> Int
monthToInt month =
    List.foldl
        (\m ( found, i ) ->
            if found || m == month then
                ( True, i )

            else
                ( False, i + 1 )
        )
        ( False, 1 )
        months
        |> Tuple.second
