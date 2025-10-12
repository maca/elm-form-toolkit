module Chapters.FormattingAndValidation exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html
import Support.CreditCardForm as CreditCardForm
import Task
import Time


type alias Book x =
    { x | formattingAndValidation : Model }


type alias Model =
    { creditCard : CreditCardForm.Model }


type Msg
    = CreditCardDemoUpdated CreditCardForm.Msg (Maybe Time.Posix)


init : Model
init =
    { creditCard = CreditCardForm.init }


update : Msg -> Book x -> ( Book x, Cmd (ElmBook.Msg (Book x)) )
update msg model =
    case msg of
        CreditCardDemoUpdated innerMsg Nothing ->
            ( model
            , Task.perform
                (\time ->
                    Actions.updateStateWithCmd
                        (update
                            (CreditCardDemoUpdated innerMsg
                                (Just time)
                            )
                        )
                )
                Time.now
            )

        CreditCardDemoUpdated creditCardMsg (Just now) ->
            let
                creditCard =
                    model.formattingAndValidation.creditCard
                        |> CreditCardForm.update now creditCardMsg
            in
            ( { model
                | formattingAndValidation =
                    { creditCard = creditCard }
              }
            , Task.perform (Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString creditCard.result))
            )


chapter : Chapter (Book x)
chapter =
    Chapter.chapter "Formatting and Validation"
        |> Chapter.withStatefulComponent
            (\book ->
                book.formattingAndValidation.creditCard
                    |> CreditCardForm.view
                    |> Html.map
                        (Actions.updateStateWithCmdWith
                            (\msg ->
                                update (CreditCardDemoUpdated msg Nothing)
                            )
                        )
            )
        |> Chapter.renderWithComponentList ""
