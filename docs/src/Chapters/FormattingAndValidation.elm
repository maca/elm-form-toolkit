module Chapters.FormattingAndValidation exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html
import Support.CreditCardForm as CreditCardForm
import Task


type alias Model =
    { creditCard : CreditCardForm.Model }


type Msg
    = CreditCardDemoUpdated CreditCardForm.Msg


init : Model
init =
    { creditCard = CreditCardForm.init }


update : Msg -> Model -> ( Model, Cmd (ElmBook.Msg state) )
update msg model =
    case msg of
        CreditCardDemoUpdated creditCardMsg ->
            let
                creditCard =
                    CreditCardForm.update creditCardMsg model.creditCard
            in
            ( { model | creditCard = creditCard }
            , Task.perform (Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString creditCard.result))
            )


chapter : Chapter { x | formattingAndValidation : Model }
chapter =
    Chapter.chapter "Formatting and Validation"
        |> Chapter.withStatefulComponent
            (\book ->
                book.formattingAndValidation.creditCard
                    |> CreditCardForm.view
                    |> Html.map
                        (Actions.updateStateWithCmdWith
                            (\msg state ->
                                update (CreditCardDemoUpdated msg) state.formattingAndValidation
                                    |> Tuple.mapFirst
                                        (\formattingAndValidation ->
                                            { state
                                                | formattingAndValidation =
                                                    formattingAndValidation
                                            }
                                        )
                            )
                        )
            )
        |> Chapter.renderWithComponentList ""
