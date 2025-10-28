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
    , zalgoTextField : Field Never
    }


type alias Book book =
    { book | parsingCookbook : Model }


type Msg
    = EventFieldsChanged (Field.Msg EventFields)
    | ZalgoTextFieldChanged (Field.Msg Never)


init : Model
init =
    { eventFields = eventFields
    , zalgoTextField = zalgoTextField
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

        ZalgoTextFieldChanged innerMsg ->
            let
                model =
                    book.parsingCookbook

                ( updatedField, result ) =
                    Parse.parseUpdate zalgoTextParser innerMsg book.parsingCookbook.zalgoTextField
            in
            ( { book | parsingCookbook = { model | zalgoTextField = updatedField } }
            , Task.perform (Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString result))
            )


chapter : Chapter { x | parsingCookbook : Model }
chapter =
    Chapter.chapter "Parsing Cookbook"
        |> Chapter.withStatefulComponentList
            [ ( "Zalgo Text"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.parsingCookbook.zalgoTextField
                            |> Field.toHtml ZalgoTextFieldChanged
                        ]
                        |> Html.map (updateStateWithCmdWith update)
              )
            , ( "Event Fields (Conditional)"
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


### Using `andUpdate` to transform input values


`andUpdate` is similar to `andThen`, but it can update `Field` attributes while parsing.

In this example, we use `andUpdate` to transform the input text into Zalgo text as the user types,
updating both the displayed value and the parsed result. The `toZalgoText` function adds diacritical
marks only to characters that don't already have them, and converts them to uppercase.

<component with-label="Zalgo Text"/>

```elm

zalgoTextParser : Parse.Parser Never String
zalgoTextParser =
    Parse.string
        |> Parse.andUpdate
            (\\field str ->
                ( Field.updateAttribute (Field.stringValue (toZalgoText str)) field
                , Parse.succeed str
                )
            )

```


### Conditional parsing using `andUpdate`


`andUpdate` can also toggle field visibility based on parsed values.

In this example, we use the value of the "Notify Participants" checkbox both to
make "Participant Emails" visible, and to parse the list if it is visible or
succeed with an empty list if it is not visible.



<component with-label="Event Fields (Conditional)"/>


```elm

eventParser : Parse.Parser EventFields { name : String, date : String, participants : List String }
eventParser =
    Parse.map3
        (\\participants name date ->
            { name = name
            , date = date
            , participants = participants
            }
        )
        (Parse.field NotifyParticipants Parse.bool
            |> Parse.andUpdate
                (\\field notify ->
                    ( Field.updateWithId Participants (Field.hidden (not notify)) field
                    , if notify then
                        Parse.field Participants (Parse.list Parse.string)

                      else
                        Parse.succeed []
                    )
                )
        )
        (Parse.field EventName Parse.string)
        (Parse.field EventDate Parse.posix |> Parse.map Iso8601.fromTime)



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
            , Field.repeatableMin 1
            , Field.repeatableMax 5
            , Field.identifier Participants
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
        (\participants name date ->
            { name = name
            , date = date
            , participants = participants
            }
        )
        (Parse.field NotifyParticipants Parse.bool
            |> Parse.andUpdate
                (\field notify ->
                    ( Field.updateWithId Participants (Field.hidden (not notify)) field
                    , if notify then
                        Parse.field Participants (Parse.list Parse.string)

                      else
                        Parse.succeed []
                    )
                )
        )
        (Parse.field EventName Parse.string)
        (Parse.field EventDate Parse.posix |> Parse.map Iso8601.fromTime)


zalgoTextField : Field Never
zalgoTextField =
    Field.text
        [ Field.label "Text Input"
        , Field.placeholder "Type something..."
        ]


zalgoTextParser : Parse.Parser Never String
zalgoTextParser =
    Parse.string
        |> Parse.andUpdate
            (\field str ->
                ( Field.updateAttribute (Field.stringValue (toZalgoText str)) field
                , Parse.succeed str
                )
            )


toZalgoText : String -> String
toZalgoText str =
    let
        zalgoMarks =
            [ "̵̌", "̵̟͝", "̵͚̂", "̸̎", "̴̰", "̵͉̉", "̵͙̊", "̵̤̃", "̶͜", "̷̱̐", "̷̹̿" ]

        isCombiningMark char =
            let
                code =
                    Char.toCode char
            in
            (code >= 0x0300 && code <= 0x036F)
                || (code >= 0x1AB0 && code <= 0x1AFF)
                || (code >= 0x1DC0 && code <= 0x1DFF)
                || (code >= 0x20D0 && code <= 0x20FF)
                || (code >= 0xFE20 && code <= 0xFE2F)

        getMark char =
            zalgoMarks
                |> List.drop (modBy (List.length zalgoMarks) (Char.toCode char))
                |> List.head
                |> Maybe.withDefault ""
                |> String.toList

        transformIfPlain chars =
            case chars of
                [] ->
                    []

                char :: rest ->
                    if Char.isAlpha char then
                        case rest of
                            nextChar :: _ ->
                                if isCombiningMark nextChar then
                                    char :: transformIfPlain rest

                                else
                                    Char.toUpper char :: getMark char ++ transformIfPlain rest

                            [] ->
                                Char.toUpper char :: getMark char

                    else
                        char :: transformIfPlain rest
    in
    str
        |> String.toList
        |> transformIfPlain
        |> String.fromList
