module Chapters.ParsingCookbook exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions exposing (updateStateWithCmdWith)
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html
import Html.Attributes as Attr
import Iso8601
import Task


type alias Model =
    { eventFields : Field EventFields
    }


type alias Book book =
    { book | parsingCookbook : Model }


type Msg
    = EventFieldsChanged (Field.Msg EventFields)


init : Model
init =
    { eventFields = eventFields
    }


update : Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update msg book =
    case msg of
        EventFieldsChanged innerMsg ->
            let
                model =
                    book.parsingCookbook

                ( updatedField, result ) =
                    Parse.parseUpdate eventParser innerMsg book.parsingCookbook.eventFields
            in
            ( { book | parsingCookbook = { model | eventFields = updatedField } }
            , Task.perform (Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString result))
            )


chapter : Chapter { x | parsingCookbook : Model }
chapter =
    Chapter.chapter "Parsing Cookbook"
        |> Chapter.withStatefulComponentList
            [ ( "Event Fields (Conditional)"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.parsingCookbook.eventFields
                            |> Field.toHtml EventFieldsChanged
                        ]
                        |> Html.map (updateStateWithCmdWith update)
              )
            ]
        |> Chapter.render parsingIntroMarkdown


parsingIntroMarkdown : String
parsingIntroMarkdown =
    """

## Updating field attributes while parsing


### Conditional parsing using `andUpdate`


`andUpdate` is similar to `andThen`, but it can update `Field` attributes too,
toggling, for instance, its visibility.

In this example, we use the value of the "Notify Participants" checkbox to make "Participant Emails"
visible and parse the list if it is visible, or just succeed with an empty list.


```elm
eventFields : Field EventFields
eventFields =
    Field.group []
        [ Field.group
            [ Field.class "inline-fields"
            ]
            [ Field.text
                [ Field.label "Event Name"
                , Field.required True
                , Field.identifier EventName
                ]
            , Field.date
                [ Field.label "Event Date"
                , Field.required True
                , Field.identifier EventDate
                ]
            ]
        , Field.checkbox
            [ Field.label "Notify Participants"
            , Field.identifier NotifyParticipants
            , Field.value (Value.bool True)
            ]
        , Field.repeatable
            [ Field.label "Participant Emails"
            , Field.identifier Participants
            , Field.repeatableMin 1
            , Field.repeatableMax 5
            ]
            (Field.email
                [ Field.required True
                , Field.identifier ParticipantEmail
                ]
            )
            []
        ]


eventParser : Parse.Parser EventFields { name : String, date : String, participants : List String }
eventParser =
    Parse.map3 (\\name date participants -> { name = name , date = date , participants = participants } )
        (Parse.field EventName Parse.string)
        (Parse.field EventDate Parse.posix |> Parse.map Iso8601.fromTime)
        (Parse.field NotifyParticipants Parse.bool
            |> Parse.andUpdate
                (\\rootGroup notify ->
                    { field =
                        Field.updateVisibleWithId Participants notify rootGroup
                            |> Result.withDefault rootGroup
                     , parser =
                        if notify then
                            Parse.field Participants (Parse.list Parse.string)
                        else
                            Parse.succeed []
                    }
                )
        )
```

<component with-label="Event Fields (Conditional)"/>


### Parsers nesting and `andUpdate` field context.

A `Field` represents either a group or individual field in a tree structure.

Parsers navigate this tree using `Parse.field` to find nodes by identifier.
Where `andUpdate` is placed determines which field it receives as input.

The following examples produce the same Bool result, but with different field
contexts:



```elm
fields : Field EventFields
fields =
    Field.group
        [ Field.identifier Root ]
        [ Field.checkbox
            [ Field.label "Notify Participants"
            , Field.identifier NotifyParticipants
            , Field.value (Value.bool True)
            ]
        ]


-- Example 1: andUpdate receives the Root field group
fields |>
    Parse.parse (
        Parse.field NotifyParticipants Parse.bool
            |> Parse.andUpdate
                (\\field bool ->
                    { field = field  -- field is the Root field group
                    , parser = Parse.succeed bool
                    }
                )
    )

-- Example 2: andUpdate receives the NotifyParticipants checkbox field
fields |>
    Parse.parse (
        Parse.field NotifyParticipants
            (Parse.bool
                |> Parse.andUpdate
                    (\\field bool ->
                        { field = field  -- field is the NotifyParticipants checkbox
                        , parser = Parse.succeed bool
                        }
                    )
            )
    )
```







"""


type EventFields
    = EventName
    | EventDate
    | NotifyParticipants
    | Participants
    | ParticipantEmail


eventFields : Field EventFields
eventFields =
    Field.group
        []
        [ Field.group
            [ Field.class "inline-fields"
            ]
            [ Field.text
                [ Field.label "Event Name"
                , Field.required True
                , Field.identifier EventName
                ]
            , Field.date
                [ Field.label "Event Date"
                , Field.required True
                , Field.identifier EventDate
                ]
            ]
        , Field.checkbox
            [ Field.label "Notify Participants"
            , Field.identifier NotifyParticipants
            , Field.value (Value.bool True)
            ]
        , Field.repeatable
            [ Field.label "Participant Emails"
            , Field.identifier Participants
            , Field.repeatableMin 1
            , Field.repeatableMax 5
            ]
            (Field.email
                [ Field.required True
                , Field.identifier ParticipantEmail
                ]
            )
            []
        ]


eventParser : Parse.Parser EventFields { name : String, date : String, participants : List String }
eventParser =
    Parse.map3
        (\name date participants ->
            { name = name
            , date = date
            , participants = participants
            }
        )
        (Parse.field EventName Parse.string)
        (Parse.field EventDate Parse.posix |> Parse.map Iso8601.fromTime)
        (Parse.field NotifyParticipants Parse.bool
            |> Parse.andUpdate
                (\field notify ->
                    { field =
                        Field.updateVisibleWithId Participants notify field
                            |> Result.withDefault field
                    , parser =
                        if notify then
                            Parse.field Participants (Parse.list Parse.string)

                        else
                            Parse.succeed []
                    }
                )
        )
