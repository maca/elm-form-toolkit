module FieldTest exposing (suite)

import Expect
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Test exposing (..)


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
                            (Parse.parse
                                (Parse.map2 Tuple.pair
                                    (Parse.field "NestedField" Parse.string)
                                    (Parse.field "NestedField2" Parse.string)
                                )
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
                            (Parse.parse
                                (Parse.map2 Tuple.pair
                                    (Parse.field "NestedField" Parse.string)
                                    (Parse.field "NestedField2" Parse.string)
                                )
                            )
                        |> Expect.equal (Ok ( "Updated value", "Value2" ))
            , test "it preserves identifier" <|
                \_ ->
                    input
                        |> Field.updateWithId "NestedField"
                            (Field.updateAttribute (Field.identifier "OtherField"))
                        |> Result.andThen
                            (Parse.parse
                                (Parse.map2 Tuple.pair
                                    (Parse.field "NestedField" Parse.string)
                                    (Parse.field "NestedField2" Parse.string)
                                )
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
        ]
