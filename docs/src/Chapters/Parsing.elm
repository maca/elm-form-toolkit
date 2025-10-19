module Chapters.Parsing exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import Html
import Html.Attributes as Attr
import Task


type alias Book book =
    { book | parsing : Model }


type Msg
    = PersonFormChanged (Field.Msg PersonFields)


type alias Model =
    { personForm : Field PersonFields
    }


type PersonFields
    = FirstName
    | LastName


init : Model
init =
    { personForm = personForm
    }


update : Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update msg book =
    let
        model =
            book.parsing

        ( newModel, cmd ) =
            case msg of
                PersonFormChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate personParser fieldMsg model.personForm
                    in
                    ( { model | personForm = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )
    in
    ( { book | parsing = newModel }, cmd )


chapter : Chapter (Book book)
chapter =
    Chapter.chapter "Parsing"
        |> Chapter.withStatefulComponentList
            [ ( "Person Form (Custom Type ID)"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.parsing.personForm
                            |> Field.toHtml PersonFormChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            ]
        |> Chapter.render markdownContent


markdownContent : String
markdownContent =
    """

The parsing mechanism is meant to emulate Json.Decode closelly, except that it
references fields not by json keys but by an arbitrary identifiers associated
with  given field.

    
## Custom Type Identifiers

Identifiers can be anything, even a string. Identifiers are used to traverse,
similar to using Json.Decode.field, the better approach is to use custom types
as identifiers.

Custom type identifiers mitigate the risk of typos by ensuring type constraints.
The compiler will catch any incorrect field references at compile time, making
your code more robust and preventing runtime errors.

```elm
type PersonFields
    = FirstName
    | LastName


personForm : Field PersonFields
personForm =
    Field.group
        [ Field.label "Person Information"
        , Field.class "inline-fields"
        ]
        [ Field.text
            [ Field.label "First Name"
            , Field.required True
            , Field.identifier FirstName
            ]
        , Field.text
            [ Field.label "Last Name"
            , Field.required True
            , Field.identifier LastName
            ]
        ]


personParser : Parse.Parser PersonFields { firstName : String, lastName : String }
personParser =
    Parse.map2 (\\first last -> { firstName = first, lastName = last })
        (Parse.field FirstName Parse.string)
        (Parse.field LastName Parse.string)
```

<component with-label="Person Form (Custom Type ID)"/>

"""


personForm : Field PersonFields
personForm =
    Field.group
        [ Field.label "Person Information"
        , Field.class "inline-fields"
        ]
        [ Field.text
            [ Field.label "First Name"
            , Field.required True
            , Field.identifier FirstName
            ]
        , Field.text
            [ Field.label "Last Name"
            , Field.required True
            , Field.identifier LastName
            ]
        ]


personParser : Parse.Parser PersonFields { firstName : String, lastName : String }
personParser =
    Parse.map2 (\first last -> { firstName = first, lastName = last })
        (Parse.field FirstName Parse.string)
        (Parse.field LastName Parse.string)
