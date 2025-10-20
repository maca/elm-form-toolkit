module IntegrationTest exposing (suite)

import Expect
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html.Attributes as Attrs
import Support.ExampleInputs exposing (..)
import Support.Interaction as Interaction exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag)
import Time


suite : Test
suite =
    describe "Integration Tests"
        [ dateFieldTests
        , datetimeFieldTests
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
