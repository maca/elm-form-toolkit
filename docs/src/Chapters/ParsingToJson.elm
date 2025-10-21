module Chapters.ParsingToJson exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html
import Support.JsonPostForm as JsonPostForm
import Task


type alias Book book =
    { book | parsingToJson : Model }


type Msg
    = JsonPostFormChanged JsonPostForm.Msg


type alias Model =
    { jsonPostForm : JsonPostForm.Model
    }


init : Model
init =
    { jsonPostForm = JsonPostForm.init
    }


update : Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update msg book =
    let
        ( newModel, cmd ) =
            case msg of
                JsonPostFormChanged innerMsg ->
                    let
                        model =
                            book.parsingToJson

                        ( updatedJsonPostForm, innerCmd ) =
                            JsonPostForm.update innerMsg model.jsonPostForm
                    in
                    ( { model | jsonPostForm = updatedJsonPostForm }
                    , Cmd.batch
                        [ Cmd.map (JsonPostFormChanged >> Actions.updateStateWithCmdWith update) innerCmd
                        , Task.perform (Actions.logActionWithString "Demo")
                            (Task.succeed "Press Submit to perform a request and see results")
                        ]
                    )
    in
    ( { book | parsingToJson = newModel }, cmd )


chapter : Chapter (Book book)
chapter =
    Chapter.chapter "Parsing to JSON"
        |> Chapter.withStatefulComponentList
            [ ( "JSON Post Form"
              , \book ->
                    book.parsingToJson.jsonPostForm
                        |> JsonPostForm.view
                        |> Html.map (JsonPostFormChanged >> Actions.updateStateWithCmdWith update)
              )
            ]
        |> Chapter.render markdownContent


markdownContent : String
markdownContent =
    """

For forms whose input is not to be parsed to an Elm type, it's possible to parse
straight to a `Json.Encode.Value` with keys corresponding to the `Field.name`
attribute by using `Parse.json`. There's no need to set `Field.identifier`
attributes.

This value can then be sent straight to a server without any prior processing.


<component with-label="JSON Post Form"/>


In this case, `Field.identifier` will not be needed, which is the reason behind
its optionality. The type signature of the form fields can be
more unconstrained.


"""
