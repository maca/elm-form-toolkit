module IntegrationTest exposing (suite)

import Expect
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html.Attributes as Attrs
import Support.Interaction as Interaction exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, containing, tag, text)
import Time


suite : Test
suite =
    describe "Integration Tests"
        [ dateFieldTests
        , datetimeFieldTests
        , validationFocusBlurTests
        , conditionalRepeatableFieldTests
        ]


dateFieldTests : Test
dateFieldTests =
    describe "date field integration" <|
        [ test "fills date field and parses to correct posix value" <|
            \_ ->
                let
                    dateField =
                        Field.date
                            [ Field.label "Date Field"
                            , Field.name "date-field"
                            , Field.required True
                            ]

                    { result } =
                        Interaction.init Parse.posix dateField
                            |> fillInput "date-field" "2023-12-25"
                in
                case result of
                    Ok posixTime ->
                        Expect.equal (Time.posixToMillis posixTime) 1703462400000

                    Err _ ->
                        Expect.fail "Expected successful parsing of date"
        , test "sets posix value and displays correct string representation" <|
            \_ ->
                let
                    testPosix =
                        Time.millisToPosix 1704067200000

                    dateField =
                        Field.date
                            [ Field.label "Date Field"
                            , Field.name "date-field"
                            , Field.value (Value.time testPosix)
                            , Field.required True
                            ]
                in
                dateField
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Query.find [ tag "input" ]
                    |> Query.has [ attribute (Attrs.attribute "value" "2024-01-01T00:00:00.000") ]
        ]


datetimeFieldTests : Test
datetimeFieldTests =
    describe "datetime field integration" <|
        [ test "fills datetime field and parses to correct posix value" <|
            \_ ->
                let
                    datetimeField =
                        Field.datetime
                            [ Field.label "Datetime Field"
                            , Field.name "datetime-field"
                            , Field.required True
                            ]

                    { result } =
                        Interaction.init Parse.posix datetimeField
                            |> fillInput "datetime-field" "2023-12-25T14:30"
                in
                case result of
                    Ok posixTime ->
                        Expect.equal (Time.posixToMillis posixTime) 1703514600000

                    Err _ ->
                        Expect.fail "Expected successful parsing of datetime"
        , test "sets posix value and displays correct string representation" <|
            \_ ->
                let
                    -- January 1, 2024 at 12:00 UTC
                    testPosix =
                        Time.millisToPosix 1704110400000

                    datetimeField =
                        Field.datetime
                            [ Field.label "Datetime Field"
                            , Field.name "datetime-field"
                            , Field.value (Value.time testPosix)
                            , Field.required True
                            ]
                in
                datetimeField
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Query.find [ tag "input" ]
                    |> Query.has [ attribute (Attrs.attribute "value" "2024-01-01T12:00:00.000") ]
        ]


validationFocusBlurTests : Test
validationFocusBlurTests =
    describe "validation focus and blur behavior" <|
        [ test "IsBlank error only appears after field is blurred when required" <|
            \_ ->
                Field.int
                    [ Field.required True
                    , Field.name "the-field"
                    ]
                    |> Interaction.init Parse.int
                    |> fillInput "the-field" ""
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.hasNot [ class "errors" ]
                        , blur "the-field"
                            >> .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Should be provided" ] ]
                        ]
        , test "Range validation errors appear regardless of focus/blur status" <|
            \_ ->
                Field.int
                    [ Field.required True
                    , Field.name "range-field"
                    , Field.min (Value.int 10)
                    , Field.max (Value.int 20)
                    ]
                    |> Interaction.init Parse.int
                    |> fillInput "range-field" "25"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Should be between 10 and 20" ] ]
                        , blur "range-field"
                            >> .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Should be between 10 and 20" ] ]
                        ]
        , test "PatternError only appears after field is blurred" <|
            \_ ->
                Field.text
                    [ Field.name "pattern-field"
                    , Field.pattern "({d}{d}{d}) {d}{d}{d}-{d}{d}{d}{d}"
                    ]
                    |> Interaction.init Parse.string
                    |> fillInput "pattern-field" "invalid text"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.hasNot [ class "errors" ]
                        , blur "pattern-field"
                            >> .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Doesn't match the required pattern" ] ]
                        ]
        , test "Valid input gets formatted according to pattern with no errors" <|
            \_ ->
                Field.text
                    [ Field.name "phone-field"
                    , Field.pattern "{d}{d}-{d}{d}"
                    ]
                    |> Interaction.init Parse.string
                    |> fillInput "phone-field" "1234"
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ tag "input" ]
                            >> Query.has [ attribute (Attrs.attribute "value" "12-34") ]
                        , .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.hasNot [ class "errors" ]
                        , blur "phone-field"
                            >> .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.hasNot [ class "errors" ]
                        ]
        ]


conditionalRepeatableFieldTests : Test
conditionalRepeatableFieldTests =
    let
        eventField =
            Field.group []
                [ Field.text
                    [ Field.label "Event Name"
                    , Field.identifier "event-name"
                    , Field.name "event-name"
                    , Field.required True
                    ]
                , Field.checkbox
                    [ Field.label "Notify Participants"
                    , Field.identifier "notify-participants"
                    , Field.name "notify-participants"
                    , Field.value (Value.bool True)
                    ]
                , Field.repeatable
                    [ Field.label "Participants"
                    , Field.repeatableMin 1
                    , Field.identifier "participants"
                    ]
                    (Field.text
                        [ Field.required True
                        , Field.identifier "participant-name"
                        , Field.name "participant-name"
                        ]
                    )
                    []
                ]

        eventParser =
            Parse.map2
                (\participants name ->
                    { name = name
                    , participants = participants
                    }
                )
                (Parse.field "notify-participants" Parse.bool
                    |> Parse.andUpdate
                        (\field notify ->
                            { field =
                                Field.updateWithId "participants" (Field.hidden (not notify)) field
                            , parser =
                                if notify then
                                    Parse.field "participants" (Parse.list Parse.string)

                                else
                                    Parse.succeed []
                            }
                        )
                )
                (Parse.field "event-name" Parse.string)
    in
    describe "conditional repeatable field with checkbox" <|
        [ test "when checkbox is checked, repeatable fields are shown and produce errors when empty" <|
            \_ ->
                let
                    interaction =
                        Interaction.init eventParser eventField
                            |> fillInput "event-name" "Team Meeting"
                            |> clickButton "Add Participants"
                            |> check "notify-participants" True
                            |> blur "participant-name"
                in
                interaction
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Should be provided" ] ]
                        , .result
                            >> Expect.err
                        ]
        , test "when checkbox is unchecked, repeatable fields are hidden and no errors are produced" <|
            \_ ->
                let
                    interaction =
                        Interaction.init eventParser eventField
                            |> fillInput "event-name" "Team Meeting"
                            |> clickButton "Add Participants"
                            |> check "notify-participants" False
                            |> blur "participant-name"
                in
                interaction
                    |> Expect.all
                        [ .field
                            >> Field.toHtml (always never)
                            >> Query.fromHtml
                            >> Query.hasNot [ class "errors" ]
                        , .result
                            >> Expect.equal
                                (Ok
                                    { name = "Team Meeting"
                                    , participants = []
                                    }
                                )
                        ]
        ]
