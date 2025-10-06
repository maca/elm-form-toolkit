module Chapters.Parsing exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions exposing (updateStateWithCmdWith)
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html
import Html.Attributes as Attr
import Iso8601
import Support.ShipmentForm as ShipmentForm
import Task
import Time


type alias Model =
    { shipmentDemo : ShipmentForm.Model
    , eventFields : Field String
    , eventFields2 : Field EventFields
    , eventFields3 : Field EventFields
    }


type alias Book book =
    { book | parsing : Model }


type Msg
    = ShipmentDemoMsg ShipmentForm.Msg
    | EventFieldsChanged (Field.Msg String)
    | EventFields2Changed (Field.Msg EventFields)
    | EventFields3Changed (Field.Msg EventFields)


init : Model
init =
    { shipmentDemo = ShipmentForm.init
    , eventFields = eventFields
    , eventFields2 = eventFields2
    , eventFields3 = eventFields3
    }


update : Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update msg book =
    let
        model =
            book.parsing
    in
    case msg of
        ShipmentDemoMsg innerMsg ->
            let
                shipmentDemo =
                    ShipmentForm.update innerMsg book.parsing.shipmentDemo
            in
            ( { book | parsing = { model | shipmentDemo = shipmentDemo } }
            , Cmd.none
            )

        EventFieldsChanged innerMsg ->
            let
                ( updatedField, result ) =
                    Parse.parseUpdate eventParser innerMsg book.parsing.eventFields
            in
            ( { book
                | parsing = { model | eventFields = updatedField }
              }
            , Task.perform (Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString result))
            )

        EventFields2Changed innerMsg ->
            let
                ( updatedField, result ) =
                    Parse.parseUpdate eventParser2 innerMsg book.parsing.eventFields2
            in
            ( { book
                | parsing = { model | eventFields2 = updatedField }
              }
            , Task.perform (Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString result))
            )

        EventFields3Changed innerMsg ->
            let
                ( updatedField, result ) =
                    Parse.parseUpdate eventParser3 innerMsg book.parsing.eventFields3
            in
            ( { book | parsing = { model | eventFields3 = updatedField } }
            , Task.perform (Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString result))
            )


chapter : Chapter { x | parsing : Model }
chapter =
    Chapter.chapter "Parsing"
        |> Chapter.withStatefulComponentList
            [ ( "Event Fields (String ID)"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.parsing.eventFields
                            |> Field.toHtml EventFieldsChanged
                        ]
                        |> Html.map (updateStateWithCmdWith update)
              )
            , ( "Event Fields (Custom Type ID)"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.parsing.eventFields2
                            |> Field.toHtml EventFields2Changed
                        ]
                        |> Html.map (updateStateWithCmdWith update)
              )
            , ( "Event Fields (Conditional)"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.parsing.eventFields3
                            |> Field.toHtml EventFields3Changed
                        ]
                        |> Html.map (updateStateWithCmdWith update)
              )
            , ( "Advanced Parsing - Shipment Form"
              , \book ->
                    book.parsing.shipmentDemo
                        |> ShipmentForm.view
                        |> Html.map (ShipmentDemoMsg >> updateStateWithCmdWith update)
              )
            ]
        |> Chapter.render parsingIntroMarkdown


parsingIntroMarkdown : String
parsingIntroMarkdown =
    """

### Using String identifiers

Identifiers can be anything, even a string. Identifiers are used to traverse,
similar to using Json.Decode.field. Use `Parse.maybe` to handle optional fields that may be empty.


```elm
eventFields : Field String
eventFields =
    Field.group
        [  Field.class "inline-fields"
        ]
        [ Field.text
            [ Field.label "Event Name"
            , Field.required True
            , Field.identifier "event-name"
            ]
        , Field.date
            [ Field.label "Event Date"
            , Field.required True
            , Field.identifier "event-date"
            ]
        , Field.int
            [ Field.label "Max Attendees (Optional)"
            , Field.identifier "max-attendees"
            , Field.min (Value.int 1)
            , Field.max (Value.int 1000)
            ]
        ]



eventParser : Parse.Parser String { eventName : String, date : Time.Posix, maxAttendees : Maybe Int }
eventParser =
    Parse.map3 (\\name date attendees -> { eventName = name, date = date, maxAttendees = attendees })
        (Parse.field "event-name" Parse.string)
        (Parse.field "event-date" Parse.posix)
        (Parse.field "max-attendees" (Parse.maybe Parse.int))
```

<component with-label="Event Fields (String ID)"/>



### Using custom type identifiers

Custom type identifiers mitigate the risk of typos by ensuring type constraints. The compiler will catch any incorrect field references at compile time, making your code more robust and preventing runtime errors.


```elm
type EventFields
    = EventName
    | EventDate
    | MaxAttendees


eventFields : Field EventFields
eventFields =
    Field.group
        [ Field.label "Event Registration"
        , Field.class "inline-fields"
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
        , Field.int
            [ Field.label "Max Attendees (Optional)"
            , Field.identifier MaxAttendees
            , Field.min (Value.int 1)
            , Field.max (Value.int 1000)
            ]
        ]


eventParser : Parse.Parser EventFields { eventName : String, date : Time.Posix, maxAttendees : Maybe Int }
eventParser =
    Parse.map3 (\\name date attendees -> { eventName = name, date = date, maxAttendees = attendees })
        (Parse.field EventName Parse.string)
        (Parse.field EventDate Parse.posix)
        (Parse.field MaxAttendees (Parse.maybe Parse.int))
```

<component with-label="Event Fields (Custom Type ID)"/>


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



### Parsers nesting and `andUpdate` field context.



## andMap parser Pipeline


### Complex Form Structure

```elm
type alias Shipment =
    { shipping : Address
    , recipients : List Recipient
    }

type alias Address =
    { firstName : String
    , lastName : String
    , address : String
    , address2 : String
    , postalCode : String
    , state : String
    , country : Countries.Country
    }

type alias Recipient =
    { email : String
    , name : String
    }
```

### Parser with andMap Pipeline


### Custom Parsing with andThen


<component with-label="Advanced Parsing - Shipment Form"/>


"""


eventFields : Field String
eventFields =
    Field.group
        [ Field.label "Event Registration"
        , Field.class "inline-fields"
        ]
        [ Field.text
            [ Field.label "Event Name"
            , Field.required True
            , Field.identifier "event-name"
            ]
        , Field.date
            [ Field.label "Event Date"
            , Field.required True
            , Field.identifier "event-date"
            ]
        , Field.int
            [ Field.label "Max Attendees (Optional)"
            , Field.identifier "max-attendees"
            , Field.min (Value.int 1)
            , Field.max (Value.int 1000)
            ]
        ]


eventParser : Parse.Parser String { eventName : String, date : Time.Posix, maxAttendees : Maybe Int }
eventParser =
    Parse.map3
        (\name date attendees ->
            { eventName = name
            , date = date
            , maxAttendees = attendees
            }
        )
        (Parse.field "event-name" Parse.string)
        (Parse.field "event-date" Parse.posix)
        (Parse.field "max-attendees" (Parse.maybe Parse.int))


type EventFields
    = EventName
    | EventDate
    | MaxAttendees
    | NotifyParticipants
    | Participants
    | ParticipantEmail


eventFields2 : Field EventFields
eventFields2 =
    Field.group
        [ Field.label "Event Registration"
        , Field.class "inline-fields"
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
        , Field.int
            [ Field.label "Max Attendees (Optional)"
            , Field.identifier MaxAttendees
            , Field.min (Value.int 1)
            , Field.max (Value.int 1000)
            ]
        ]


eventParser2 : Parse.Parser EventFields { eventName : String, date : Time.Posix, maxAttendees : Maybe Int }
eventParser2 =
    Parse.map3
        (\name date attendees ->
            { eventName = name
            , date = date
            , maxAttendees = attendees
            }
        )
        (Parse.field EventName Parse.string)
        (Parse.field EventDate Parse.posix)
        (Parse.field MaxAttendees (Parse.maybe Parse.int))


eventFields3 : Field EventFields
eventFields3 =
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


eventParser3 : Parse.Parser EventFields { name : String, date : String, participants : List String }
eventParser3 =
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
