module FieldTest exposing (suite)

import Expect
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Json.Decode as Decode
import Json.Encode as Encode
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
        , describe "setValues"
            [ test "sets values from JSON with two-level structure" <|
                \_ ->
                    testForm
                        |> Field.setValues sampleValues
                        |> Result.andThen
                            (\updatedForm ->
                                updatedForm
                                    |> Parse.parse
                                        (Parse.map4
                                            (\firstName lastName streetName postalCode ->
                                                { firstName = firstName
                                                , lastName = lastName
                                                , streetName = streetName
                                                , postalCode = postalCode
                                                }
                                            )
                                            (Parse.field "FirstName" Parse.string)
                                            (Parse.field "LastName" Parse.string)
                                            (Parse.field "StreetName" Parse.string)
                                            (Parse.field "PostalCode" Parse.string)
                                        )
                            )
                        |> Expect.equal
                            (Ok
                                { firstName = "José"
                                , lastName = "García"
                                , streetName = "Avenida Revolución"
                                , postalCode = "03100"
                                }
                            )
            ]
        ]


testForm : Field.Field String
testForm =
    Field.group []
        [ Field.group
            [ Field.name "recipient" ]
            [ Field.text
                [ Field.name "first-name"
                , Field.identifier "FirstName"
                ]
            , Field.text
                [ Field.name "last-name"
                , Field.identifier "LastName"
                ]
            ]
        , Field.group
            [ Field.name "address" ]
            [ Field.text
                [ Field.name "street-name"
                , Field.identifier "StreetName"
                ]
            , Field.text
                [ Field.name "postal-code"
                , Field.identifier "PostalCode"
                ]
            ]
        ]


sampleValuesJson : String
sampleValuesJson =
    """{
  "recipient": {
    "first-name": "José",
    "last-name": "García"
  },
  "address": {
    "street-name": "Avenida Revolución",
    "postal-code": "03100"
  }
}"""


sampleValues : Encode.Value
sampleValues =
    case Decode.decodeString Decode.value sampleValuesJson of
        Ok value ->
            value
        
        Err _ ->
            Encode.null
