module Chapters.FormattingAndValidation exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions exposing (updateStateWithCmdWith)
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.View as View
import Html exposing (Html)
import Html.Attributes as Attrs exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)
import Result
import Support.CreditCardForm as CreditCardForm
import Task



-- chapter : Chapter { x | formattingAndValidation : Model }


chapter =
    Chapter.chapter "Formatting and Validation"
        |> Chapter.withStatefulComponent
            (\{ formattingAndValidation } ->
                view formattingAndValidation
                    |> Html.map
                        (updateStateWithCmdWith
                            (\msg state ->
                                update msg state.formattingAndValidation
                                    |> Tuple.mapFirst
                                        (\formattingAndValidation_ ->
                                            { state | formattingAndValidation = formattingAndValidation_ }
                                        )
                            )
                        )
            )
        |> Chapter.renderWithComponentList ""


type alias Model =
    { formFields : Field CreditCardForm.CardFields
    , cardInformation : Maybe CreditCardForm.CardInformation
    }


type Msg
    = FormChanged (Field.Msg CreditCardForm.CardFields)
    | FormSubmitted


init : Model
init =
    { formFields = CreditCardForm.cardFields
    , cardInformation = Nothing
    }


update : Msg -> Model -> ( Model, Cmd (ElmBook.Msg state) )
update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( formFields, result ) =
                    model.formFields
                        |> Parse.parseUpdate
                            CreditCardForm.cardInformationParser
                            inputMsg
            in
            ( { model
                | formFields = formFields
                , cardInformation = Result.toMaybe result
              }
            , Task.perform (ElmBook.Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString result))
            )

        FormSubmitted ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.h1 [] [ Html.text "Formatting and Validation" ]
        , Html.p []
            [ Html.text "This chapter demonstrates how to implement custom formatting and validation for form fields. "
            , Html.text "The credit card form below shows automatic formatting (spaces every 4 digits) and Luhn algorithm validation."
            ]
        , Html.div
            [ Attrs.class "milligram" ]
            [ Html.form
                [ onSubmit FormSubmitted
                , novalidate True
                ]
                [ View.fromField FormChanged model.formFields |> View.toHtml
                , Html.button [ onClick FormSubmitted ] [ Html.text "Submit" ]
                ]
            ]
        , Html.div
            [ Attrs.style "margin-top" "20px" ]
            [ Html.h3 [] [ Html.text "How it works:" ]
            , Html.ul []
                [ Html.li [] [ Html.text "Card numbers are automatically formatted with spaces every 4 digits" ]
                , Html.li [] [ Html.text "Only numeric characters are allowed in the card number field" ]
                , Html.li [] [ Html.text "The Luhn algorithm validates the card number checksum" ]
                , Html.li [] [ Html.text "Try entering: 4532015112830366 (a valid test card number)" ]
                ]
            ]
        ]
