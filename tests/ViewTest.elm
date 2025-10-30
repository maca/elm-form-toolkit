module ViewTest exposing (suite)

import Expect
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html.Attributes as Attrs exposing (for, name, required)
import Support.ExampleInputs exposing (..)
import Support.Interaction as Interaction exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector
    exposing
        ( attribute
        , class
        , containing
        , id
        , tag
        , text
        )


suite : Test
suite =
    describe "View"
        [ stringFieldTests
        , strictAutocompleteFieldTests
        , checkboxFieldTests
        , selectFieldTests
        , radioFieldTests
        , repeatableFieldsTests
        ]


stringFieldTests : Test
stringFieldTests =
    describe "string field" <|
        [ test "label and hint" <|
            \_ ->
                stringInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ tag "input" ]
                            >> Query.has
                                [ attribute
                                    (Attrs.attribute "aria-describedby" "string-field-hint")
                                , attribute (Attrs.placeholder "String value")
                                ]
                        , Query.find [ tag "label" ]
                            >> Query.has
                                [ text "Enter your string"
                                , id "string-field-label"
                                , attribute (for "string-field")
                                ]
                        , Query.find [ id "string-field-hint" ]
                            >> Query.has [ text "Must be a string" ]
                        ]
        , test "class" <|
            \_ ->
                stringInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has
                            [ class "field"
                            , class "styled-string-field"
                            ]
                        ]
        , test "validation feedback" <|
            \_ ->
                let
                    { field } =
                        Interaction.init Parse.string blankInput
                            |> blur "blank-field"
                in
                field
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ class "required" ]
                        , Query.has
                            [ containing
                                [ tag "input"
                                , attribute (Attrs.attribute "aria-invalid" "true")
                                ]
                            ]
                        , Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Should be provided" ] ]
                        ]
        , test "datalist options" <|
            \_ ->
                stringInputWithOptions
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has
                            [ attribute (Attrs.attribute "list" "string-field-datalist")
                            , attribute (Attrs.attribute "autocomplete" "on")
                            ]
                        , Query.find [ tag "datalist" ]
                            >> Expect.all
                                [ Query.has
                                    [ id "string-field-datalist"
                                    , attribute (Attrs.attribute "role" "listbox")
                                    ]
                                , Query.has
                                    [ containing
                                        [ tag "option"
                                        , attribute (Attrs.attribute "value" "red")
                                        ]
                                    ]
                                , Query.has
                                    [ containing
                                        [ tag "option"
                                        , attribute (Attrs.attribute "value" "green")
                                        ]
                                    ]
                                , Query.has
                                    [ containing
                                        [ tag "option"
                                        , attribute (Attrs.attribute "value" "blue")
                                        ]
                                    ]
                                ]
                        ]
        ]


strictAutocompleteFieldTests : Test
strictAutocompleteFieldTests =
    describe "strict autocomplete" <|
        [ test "sets value" <|
            \_ ->
                let
                    { result } =
                        Field.strictAutocomplete
                            [ Field.name "language"
                            , Field.stringOptions [ "Español", "English", "Deutsch" ]
                            ]
                            |> Interaction.init Parse.string
                            |> fillInput "language" "Español"
                in
                Expect.equal result (Ok "Español")
        , test "restricts value" <|
            \_ ->
                let
                    { result } =
                        Field.strictAutocomplete
                            [ Field.name "language"
                            , Field.stringOptions [ "Español", "English", "Deutsch" ]
                            ]
                            |> Interaction.init Parse.value
                            |> fillInput "language" "Else"
                in
                Expect.equal result (Ok Value.blank)
        , test "errors with when no options are provided" <|
            \_ ->
                Field.strictAutocomplete []
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Query.find [ class "errors" ]
                    |> Query.has [ containing [ text "No options have been provided" ] ]
        ]


checkboxFieldTests : Test
checkboxFieldTests =
    describe "checkbox" <|
        [ test "label references input" <|
            \_ ->
                checkboxInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ tag "input" ]
                            >> Query.has
                                [ attribute
                                    (Attrs.attribute "aria-describedby" "checkbox-hint")
                                ]
                        , Query.find [ tag "label" ]
                            >> Query.has
                                [ text "Accept"
                                , id "checkbox-label"
                                , attribute (for "checkbox")
                                ]
                        , Query.find [ id "checkbox-hint" ]
                            >> Query.has [ text "You have to check the box" ]
                        ]
        ]


selectFieldTests : Test
selectFieldTests =
    describe "select" <|
        [ test "label references input" <|
            \_ ->
                selectInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ tag "label" ]
                            >> Query.has [ text "Language" ]
                        , Query.find [ tag "label" ]
                            >> Query.has [ attribute (for "select") ]
                        , Query.find [ tag "select" ]
                            >> Query.has
                                [ attribute (Attrs.attribute "aria-labelledby" "select-label") ]
                        , Query.find [ tag "select" ]
                            >> Query.has
                                [ attribute (Attrs.attribute "aria-describedby" "select-hint") ]
                        , Query.find [ tag "select" ]
                            >> Query.hasNot
                                [ attribute (Attrs.attribute "aria-invalid" "true") ]
                        , Query.has [ class "required" ]
                        ]
        , test "options" <|
            \_ ->
                selectInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ tag "option", containing [ text "Español" ] ]
                            >> Query.has [ tag "option" ]
                        , Query.find [ tag "option", containing [ text "English" ] ]
                            >> Query.has [ tag "option" ]
                        , Query.find [ tag "option", containing [ text "Deutsch" ] ]
                            >> Query.has [ tag "option" ]
                        ]
        , test "has errors if no options are provided" <|
            \_ ->
                Field.select []
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Query.find [ class "errors" ]
                    |> Query.has [ containing [ text "No options have been provided" ] ]
        , test "shows validation feedback" <|
            \_ ->
                let
                    { field } =
                        Interaction.init Parse.string selectInput
                            |> blur "select"
                in
                field
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ class "required" ]
                        , Query.find [ tag "select" ]
                            >> Query.has
                                [ attribute (Attrs.attribute "aria-invalid" "true") ]
                        , Query.find [ class "errors" ]
                            >> Query.has [ containing [ text "Should be provided" ] ]
                        ]
        ]


radioFieldTests : Test
radioFieldTests =
    describe "radio" <|
        [ test "has labeled radiogroup" <|
            \_ ->
                radioInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find
                            [ tag "label"
                            , attribute (Attrs.for "radio-inputs")
                            , id "radio-inputs-label"
                            ]
                            >> Query.has [ text "Radio inputs" ]
                        , Query.find [ attribute (Attrs.attribute "role" "radiogroup") ]
                            >> Query.has
                                [ attribute
                                    (Attrs.attribute "aria-labelledby" "radio-inputs-label")
                                , attribute
                                    (Attrs.attribute "aria-describedby" "radio-inputs-hint")
                                ]
                        , Query.find [ id "radio-inputs-hint" ]
                            >> Query.has [ text "Turn the light on or off" ]
                        ]
        , test "has options described by hint" <|
            \_ ->
                radioInput
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ id "radio-inputs-hint" ]
                            >> Query.has [ text "Turn the light on or off" ]
                        , Query.find [ tag "input", id "radio-inputs-0-option" ]
                            >> Query.has
                                [ attribute (name "radio-inputs")
                                , attribute (required True)
                                ]
                        , Query.find
                            [ tag "label"
                            , attribute (Attrs.for "radio-inputs-0-option")
                            ]
                            >> Query.has [ text "On" ]
                        , Query.find [ tag "input", id "radio-inputs-1-option" ]
                            >> Query.has
                                [ attribute (name "radio-inputs")
                                , attribute (required True)
                                ]
                        , Query.find
                            [ tag "label"
                            , attribute (Attrs.for "radio-inputs-1-option")
                            ]
                            >> Query.has [ text "Off" ]
                        ]
        , test "has errors if no options are provided" <|
            \_ ->
                Field.radio []
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Query.find [ class "errors" ]
                    |> Query.has [ containing [ text "No options have been provided" ] ]
        , test "has invalid options and errors after failed validation" <|
            \_ ->
                let
                    { field } =
                        Interaction.init Parse.bool radioInput
                            |> interact
                                (Query.find [ id "radio-inputs-1-option" ])
                                Event.blur
                in
                field
                    |> Field.toHtml (always never)
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ tag "input", id "radio-inputs-0-option" ]
                            >> Query.has
                                [ attribute (name "radio-inputs")
                                , attribute (required True)
                                , attribute (Attrs.attribute "aria-invalid" "true")
                                ]
                        , Query.find [ tag "input", id "radio-inputs-1-option" ]
                            >> Query.has
                                [ attribute (name "radio-inputs")
                                , attribute (required True)
                                , attribute (Attrs.attribute "aria-invalid" "true")
                                ]
                        ]
        ]


repeatableFieldsTests : Test
repeatableFieldsTests =
    test "repeatable inputs" <|
        \_ ->
            let
                { result } =
                    Interaction.init bandParser bandFields
                        |> fillInput "band-name" "Love and Rockets"
                        |> fillInput "member-name" "Daniel Ash"
                        |> fillInput "member-age" "67"
                        |> clickButton "Add"
                        |> fillInputWithIndex 1 "member-name" "David J"
                        |> fillInputWithIndex 1 "member-age" "67"
                        |> clickButton "Add"
                        |> fillInputWithIndex 2 "member-name" "Kevin Haskins"
                        |> fillInputWithIndex 2 "member-age" "64"
            in
            Expect.equal result
                (Ok
                    { name = "Love and Rockets"
                    , members =
                        [ { name = "Daniel Ash", age = 67 }
                        , { name = "David J", age = 67 }
                        , { name = "Kevin Haskins", age = 64 }
                        ]
                    }
                )
