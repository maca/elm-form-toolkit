module IntegrationTest exposing (suite)

import Expect
import FormToolkit.Error exposing (Error(..))
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html.Attributes as Attrs
import Support.ExampleInputs exposing (..)
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
                        [ fillInput "the-field" ""
                            >> .field
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
                        [ fillInput "range-field" "25"
                            >> .field
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
        ]
