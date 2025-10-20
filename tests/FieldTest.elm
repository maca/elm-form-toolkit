module FieldTest exposing (suite)

import Expect
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Support.ExampleInputs exposing (datetimeInput)
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Field"
        [ let
            input =
                Field.group []
                    [ Field.group []
                        [ Field.text
                            [ Field.identifier "NestedField"
                            , Field.value (Value.string "Value")
                            ]
                        , Field.text
                            [ Field.identifier "NestedField2"
                            , Field.value (Value.string "Value2")
                            ]
                        ]
                    ]
          in
          describe "update attributes"
            [ test "updating single attribute succeeds" <|
                \_ ->
                    input
                        |> Field.updateWithId "NestedField"
                            (Field.updateAttribute
                                (Field.value (Value.string "Updated value"))
                            )
                        |> Result.andThen
                            (\field ->
                                Parse.parse
                                    (Parse.map2 Tuple.pair
                                        (Parse.field "NestedField" Parse.string)
                                        (Parse.field "NestedField2" Parse.string)
                                    )
                                    field
                            )
                        |> Expect.equal (Ok ( "Updated value", "Value2" ))
            , test "updating multiple attributes succeeds" <|
                \_ ->
                    input
                        |> Field.updateWithId "NestedField"
                            (Field.updateAttributes
                                [ Field.value (Value.string "Updated value") ]
                            )
                        |> Result.andThen
                            (\field ->
                                Parse.parse
                                    (Parse.map2 Tuple.pair
                                        (Parse.field "NestedField" Parse.string)
                                        (Parse.field "NestedField2" Parse.string)
                                    )
                                    field
                            )
                        |> Expect.equal (Ok ( "Updated value", "Value2" ))
            , test "it preserves identifier" <|
                \_ ->
                    input
                        |> Field.updateWithId "NestedField"
                            (Field.updateAttribute (Field.identifier "OtherField"))
                        |> Result.andThen
                            (\field ->
                                Parse.parse
                                    (Parse.map2 Tuple.pair
                                        (Parse.field "NestedField" Parse.string)
                                        (Parse.field "NestedField2" Parse.string)
                                    )
                                    field
                            )
                        |> Expect.equal (Ok ( "Value", "Value2" ))
            , test "fails when no matching id" <|
                \_ ->
                    input
                        |> Field.updateWithId "NotExisting"
                            (Field.updateAttribute
                                (Field.value (Value.string "Updated value"))
                            )
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]
        , describe "repeatable with defaults"
            [ test "uses all defaults when more than min but less than max" <|
                \_ ->
                    Field.repeatable
                        [ Field.repeatableMin 2
                        , Field.repeatableMax 5
                        ]
                        (Field.text [ Field.identifier "field" ])
                        [ \field -> Ok (Field.updateValue (Value.string "default1") field)
                        , \field -> Ok (Field.updateValue (Value.string "default2") field)
                        , \field -> Ok (Field.updateValue (Value.string "default3") field)
                        ]
                        |> Parse.parse (Parse.list Parse.string)
                        |> Expect.equal (Ok [ "default1", "default2", "default3" ])
            ]
        , describe "datetime field"
            [ test "can be created and parsed as posix" <|
                \_ ->
                    datetimeInput
                        |> Parse.parse Parse.posix
                        |> Expect.equal (Ok (Time.millisToPosix 1609459200000))
            , test "has correct input type" <|
                \_ ->
                    datetimeInput
                        |> Field.toProperties
                        |> .inputType
                        |> Expect.equal Field.LocalDatetime
            ]
        ]
