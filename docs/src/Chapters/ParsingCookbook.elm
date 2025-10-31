module Chapters.ParsingCookbook exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html
import Html.Attributes as Attr
import Iso8601
import Support.AddressForm as AddressForm
import Task


type alias Book book =
    { book | parsingCookbook : Model }


type Msg
    = PersonFormChanged (Field.Msg PersonFields)
    | AddressFormChanged AddressForm.Msg
    | EventFieldsChanged (Field.Msg EventFields)
    | ZalgoTextFieldChanged (Field.Msg Never)


type alias Model =
    { personForm : Field PersonFields
    , addressForm : AddressForm.Model
    , eventFields : Field EventFields
    , zalgoTextField : Field Never
    }


type PersonFields
    = FirstName
    | LastName


type EventFields
    = EventName
    | EventDate
    | NotifyParticipants
    | Participants
    | ParticipantEmail


init : Model
init =
    { personForm = personForm
    , addressForm = AddressForm.init
    , eventFields = eventFields
    , zalgoTextField = zalgoTextField
    }


update : Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update msg book =
    let
        model =
            book.parsingCookbook

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

                AddressFormChanged innerMsg ->
                    let
                        updatedAddressForm =
                            AddressForm.update innerMsg model.addressForm
                    in
                    ( { model | addressForm = updatedAddressForm }
                    , Task.perform (Actions.logActionWithString "Demo")
                        (Task.succeed "Press Submit to see results")
                    )

                EventFieldsChanged innerMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate eventParser innerMsg model.eventFields
                    in
                    ( { model | eventFields = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                ZalgoTextFieldChanged innerMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate zalgoTextParser innerMsg model.zalgoTextField
                    in
                    ( { model | zalgoTextField = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )
    in
    ( { book | parsingCookbook = newModel }, cmd )


chapter : Chapter (Book book)
chapter =
    Chapter.chapter "Parsing Cookbook"
        |> Chapter.withStatefulComponentList
            [ ( "Person Form (Custom Type ID)"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.parsingCookbook.personForm
                            |> Field.toHtml PersonFormChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Address Form (andMap Pipeline)"
              , \book ->
                    book.parsingCookbook.addressForm
                        |> AddressForm.view
                        |> Html.map (AddressFormChanged >> Actions.updateStateWithCmdWith update)
              )
            , ( "Event Fields (Conditional)"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.parsingCookbook.eventFields
                            |> Field.toHtml EventFieldsChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Zalgo Text"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.parsingCookbook.zalgoTextField
                            |> Field.toHtml ZalgoTextFieldChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            ]
        |> Chapter.render markdownContent


markdownContent : String
markdownContent =
    """

The parsing mechanism is meant to emulate `Json.Decode` closely, except for the
use of identifiers instead of json keys to reference fields.

The primitive parsers are `Parse.string`, `Parse.int`,
`Parse.float`, `Parse.bool`, `Parse.posix` for time, `Parse.list`, and `Parse.json`.
Use these in combination with `Parse.field` to apply to a field with a
corresponding identifier if it exists. If the field can have a blank value or be
not present use `Parse.maybe`.
`Parse.stringWithFormat` will format, validate and parse a text input with a
provided pattern.

The output of a parser can be transformed using `map`, and they can be combined
using `map2`, `map3`, and other map functions to create records or tuples. For
more complex scenarios, `andMap` can be used to apply parsers in sequence using
the applicative pattern, or `andThen` can be used to chain parsers where the second
depends on the first's result.

## Custom Type Identifiers

Identifiers are used to traverse fields, similar to using Json.Decode.field, they can
be anything, even a string, but the better approach is to use custom types as
identifiers.

Custom type identifiers mitigate the risk of typos by ensuring type constraints.
The compiler will catch any non-existing field references at compile time, making
the code more robust and preventing runtime errors.


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
    Parse.map2
        (\\first last ->
            { firstName = first
            , lastName = last
            }
        )
        (Parse.field FirstName Parse.string)
        (Parse.field LastName Parse.string)
```

<component with-label="Person Form (Custom Type ID)"/>


## Parser Pipeline with `andMap`

For complex forms with multiple fields, use the applicative pattern with `andMap` to build parsing pipelines.

<component with-label="Address Form (andMap Pipeline)"/>

```elm
import Countries

type alias Address =
    { firstName : String
    , lastName : String
    , address : String
    , addressNumber : Int
    , addressExtra : Maybe String
    , postalCode : String
    , state : String
    , country : Countries.Country
    }

shipmentAddressParser : Parse.Parser ShipmentFields Address
shipmentAddressParser =
    Parse.succeed Address
        |> Parse.andMap (Parse.field AddressFirstName Parse.string)
        |> Parse.andMap (Parse.field AddressLastName Parse.string)
        |> Parse.andMap (Parse.field AddressStreet Parse.string)
        |> Parse.andMap (Parse.field AddressNumber Parse.int)
        |> Parse.andMap (Parse.field AddressExtra (Parse.maybe Parse.string))
        |> Parse.andMap (Parse.field PostalCode Parse.string)
        |> Parse.andMap (Parse.field AddressState Parse.string)
        |> Parse.andMap shipmentCountryParser
```

The `andMap` pattern works by applying each field parser to the constructor function. Each successful parse applies one argument to the constructor, building up the final record. If any field fails to parse, the entire parser fails with error information about the specific field.

For fields that require custom validation, use `andThen` to chain validation logic:

```elm
shipmentCountryParser : Parse.Parser ShipmentFields Countries.Country
shipmentCountryParser =
    Parse.field AddressCountry
        (Parse.string
            |> Parse.andThen
                (\\countryStr ->
                    case Countries.fromCode countryStr of
                        Just country ->
                            Parse.succeed country

                        Nothing ->
                            Parse.fail "Invalid country"
                )
        )
```

A full example of a sandbox application for this form can be found [here](https://github.com/maca/form-toolkit/blob/main/docs/src/Support/AddressForm.elm).


## Updating field attributes while parsing


### Conditional parsing using `andUpdate`


`andUpdate` is similar to `andThen`, but it can update `Field` attributes while parsing.

`andUpdate` can toggle field visibility based on parsed values.

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


### Using `andUpdate` to transform input values


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
                let
                    zalgoStr =
                        toZalgoText str
                in
                ( Field.updateAttribute (Field.stringValue zalgoStr) field
                , Parse.succeed zalgoStr
                )
            )

```

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
