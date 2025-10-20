module Chapters.ParsingToJson exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html
import Html.Attributes as Attr
import Json.Encode as Encode
import Support.JsonPostForm as JsonPostForm
import Support.ViewHelpers exposing (failureDiv, successDiv)
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
        model =
            book.parsingToJson

        ( newModel, cmd ) =
            case msg of
                JsonPostFormChanged innerMsg ->
                    let
                        ( updatedJsonPostForm, innerCmd ) =
                            JsonPostForm.update innerMsg model.jsonPostForm
                    in
                    ( { model | jsonPostForm = updatedJsonPostForm }
                    , Cmd.map (JsonPostFormChanged >> Actions.updateStateWithCmdWith update) innerCmd
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


```elm
jsonForm : Field anything
jsonForm =
    Field.group []
        [ Field.text
            [ Field.label "Username"
            , Field.required True
            , Field.name "username"
            ]
        , Field.datetime
            [ Field.label "Preferred Meeting Time"
            , Field.required True
            , Field.name "meeting-time"
            ]
        , Field.int
            [ Field.label "Tolerance for Spicy Food"
            , Field.name "spicy-tolerance"
            , Field.min (Value.int 1)
            , Field.max (Value.int 10)
            ]
        ]

```

In this case, `Field.identifier` will not be needed, which is the reason behind
its optionality. The type signature of the form fields can be
more unconstrained.


"""


jsonForm : Field anything
jsonForm =
    Field.group []
        [ Field.text
            [ Field.label "Username"
            , Field.required True
            , Field.name "username"
            ]
        , Field.datetime
            [ Field.label "Preferred Meeting Time"
            , Field.required True
            , Field.name "meeting-time"
            ]
        , Field.int
            [ Field.label "Tolerance for Spicy Food"
            , Field.name "spicy-tolerance"
            , Field.min (Value.int 1)
            , Field.max (Value.int 10)
            ]
        ]
