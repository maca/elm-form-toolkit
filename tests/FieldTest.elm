module FieldTest exposing (suite)

import Expect
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html.Attributes as Attrs
import Json.Decode as Decode
import Json.Encode as Encode
import Support.ExampleInputs exposing (datetimeInput)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag)
import Time


suite : Test
suite =
    describe "Field"
        [ updateAttributesTests
        , repeatableTests
        , datetimeFieldTests
        , updateValuesFromJsonTests
        ]


updateAttributesTests : Test
updateAttributesTests =
    let
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


repeatableTests : Test
repeatableTests =
    describe "repeatable with defaults"
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


datetimeFieldTests : Test
datetimeFieldTests =
    describe "datetime field"
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


updateValuesFromJsonTests : Test
updateValuesFromJsonTests =
    describe "updateValuesFromJson"
        [ test "sets values from JSON with two-level structure" <|
            \_ ->
                case Field.updateValuesFromJson sampleValues testForm of
                    Ok updatedForm ->
                        updatedForm
                            |> Field.toHtml (always never)
                            |> Query.fromHtml
                            |> Expect.all
                                [ Query.find [ tag "input", attribute (Attrs.name "first-name") ]
                                    >> Query.has [ attribute (Attrs.value "José") ]
                                , Query.find [ tag "input", attribute (Attrs.name "last-name") ]
                                    >> Query.has [ attribute (Attrs.value "García") ]
                                , Query.find [ tag "input", attribute (Attrs.name "street-name") ]
                                    >> Query.has [ attribute (Attrs.value "Avenida Revolución") ]
                                , Query.find [ tag "input", attribute (Attrs.name "postal-code") ]
                                    >> Query.has [ attribute (Attrs.value "03100") ]
                                ]

                    Err _ ->
                        Expect.fail "updateValuesFromJson should succeed"
        , test "sets values from JSON for form with repeatable hobby fields" <|
                \_ ->
                    case Field.updateValuesFromJson hobbiesFormValues hobbiesForm of
                        Ok updatedForm ->
                            updatedForm
                                |> Field.toHtml (always never)
                                |> Query.fromHtml
                                |> Expect.all
                                    [ Query.find [ tag "input", attribute (Attrs.name "name") ]
                                        >> Query.has [ attribute (Attrs.value "Alice") ]
                                    , Query.findAll [ tag "input", attribute (Attrs.name "hobby") ]
                                        >> Query.index 0
                                        >> Query.has [ attribute (Attrs.value "reading") ]
                                    , Query.findAll [ tag "input", attribute (Attrs.name "hobby") ]
                                        >> Query.index 1
                                        >> Query.has [ attribute (Attrs.value "cycling") ]
                                    , Query.findAll [ tag "input", attribute (Attrs.name "hobby") ]
                                        >> Query.index 2
                                        >> Query.has [ attribute (Attrs.value "cooking") ]
                                    ]

                        Err _ ->
                            Expect.fail "updateValuesFromJson should succeed for repeatable fields"
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


hobbiesForm : Field.Field String
hobbiesForm =
    Field.group []
        [ Field.text
            [ Field.name "name"
            , Field.identifier "UserName"
            ]
        , Field.repeatable
            [ Field.name "hobbies"
            , Field.identifier "UserHobbies"
            ]
            (Field.text [ Field.name "hobby" ])
            [ \field -> Ok (Field.updateValue Value.blank field)
            , \field -> Ok (Field.updateValue Value.blank field)
            , \field -> Ok (Field.updateValue Value.blank field)
            ]
        ]


hobbiesFormValuesJson : String
hobbiesFormValuesJson =
    """
{
  "name": "Alice",
  "hobbies": [
    {
      "hobby": "reading"
    },
    {
      "hobby": "cycling"
    },
    {
      "hobby": "cooking"
    }
  ]
}
"""


hobbiesFormValues : Encode.Value
hobbiesFormValues =
    case Decode.decodeString Decode.value hobbiesFormValuesJson of
        Ok value ->
            value

        Err _ ->
            Encode.null
