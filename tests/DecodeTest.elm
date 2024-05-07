module DecodeTest exposing (suite)

import Expect
import FormToolkit.Decode as Decode
import FormToolkit.Input as Input exposing (Error(..))
import FormToolkit.Value as Value
import Json.Decode
import Json.Encode
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Decoding"
        [ describe "succeeds"
            [ test "decoding string" <|
                \_ ->
                    Decode.decode Decode.string stringInput
                        |> Expect.equal (Ok "A string")
            , test "decoding int" <|
                \_ ->
                    Decode.decode Decode.int intInput
                        |> Expect.equal (Ok 1)
            , test "decoding float" <|
                \_ ->
                    Decode.decode Decode.float floatInput
                        |> Expect.equal (Ok 1.1)
            , test "decoding bool" <|
                \_ ->
                    Decode.decode Decode.bool boolInput
                        |> Expect.equal (Ok True)
            , test "decoding posix" <|
                \_ ->
                    Decode.decode Decode.posix posixInput
                        |> Expect.equal (Ok (Time.millisToPosix 0))
            , test "decoding field by id" <|
                \_ ->
                    Input.group [] [ stringInput ]
                        |> Decode.decode
                            (Decode.field StringField Decode.string)
                        |> Expect.equal (Ok "A string")
            ]
        , describe "failure"
            [ describe "on simple decoder"
                [ test "produces error" <|
                    \_ ->
                        Decode.validateAndDecode Decode.int stringInput
                            |> Tuple.second
                            |> Expect.equal
                                (Err [ ParseError (Just StringField) ])
                , test "validates input" <|
                    \_ ->
                        Decode.validateAndDecode Decode.int stringInput
                            |> Tuple.first
                            |> Input.errors
                            |> Expect.equal [ ParseError (Just StringField) ]
                ]
            , let
                result =
                    Input.group [] [ stringInput ]
                        |> Decode.validateAndDecode
                            (Decode.field StringField Decode.int)
              in
              describe "on field decoding"
                [ test "produces error" <|
                    \_ ->
                        Tuple.second result
                            |> Expect.equal (Err [ ParseError (Just StringField) ])
                , test "validates input" <|
                    \_ ->
                        Tuple.first result
                            |> Input.errors
                            |> Expect.equal [ ParseError (Just StringField) ]
                ]
            ]
        , describe "encode json"
            [ test "string" <|
                \_ ->
                    Decode.decode Decode.json stringInput
                        |> Result.withDefault (Json.Encode.string "")
                        |> Json.Decode.decodeValue
                            (Json.Decode.field "string-field" Json.Decode.string)
                        |> Expect.equal (Ok "A string")
            , test "group with no name" <|
                \_ ->
                    Decode.decode Decode.json
                        (Input.group [] [ stringInput, intInput ])
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue simpleJsonDecoder
                        |> Expect.equal (Ok ( "A string", 1 ))
            , test "group name" <|
                \_ ->
                    Decode.decode Decode.json groupWithName
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue groupWithNameDecoder
                        |> Expect.equal (Ok ( "A string", 1 ))
            , test "repeatable and group with name" <|
                \_ ->
                    Decode.decode Decode.json
                        (Input.repeatable [ Input.name "repeatable" ]
                            groupWithName
                            [ groupWithName, groupWithName ]
                        )
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue
                            (Json.Decode.field "repeatable"
                                (Json.Decode.list groupWithNameDecoder)
                            )
                        |> Expect.equal
                            (Ok [ ( "A string", 1 ), ( "A string", 1 ) ])
            , test "repeatable and group with noname" <|
                \_ ->
                    Decode.decode Decode.json
                        (Input.repeatable [ Input.name "repeatable" ]
                            groupWithNoName
                            [ groupWithNoName, groupWithNoName ]
                        )
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue
                            (Json.Decode.field "repeatable"
                                (Json.Decode.list simpleJsonDecoder)
                            )
                        |> Expect.equal
                            (Ok [ ( "A string", 1 ), ( "A string", 1 ) ])
            ]
        , describe "validates"
            [ let
                result =
                    Decode.validateAndDecode (Decode.succeed ()) blankInput
              in
              describe "presence when decoding succeeds"
                [ test "produces error" <|
                    \_ ->
                        Tuple.second result
                            |> Expect.equal
                                (Err [ IsBlank (Just BlankField) ])
                , test "validates input" <|
                    \_ ->
                        Tuple.first result
                            |> Input.errors
                            |> Expect.equal [ IsBlank (Just BlankField) ]
                ]
            , let
                result =
                    Input.group [] [ blankInput ]
                        |> Decode.validateAndDecode (Decode.succeed ())
              in
              describe "nested field when not expressely decoded"
                [ test "produces error" <|
                    \_ ->
                        Tuple.second result
                            |> Expect.equal (Err [ IsBlank (Just BlankField) ])
                , test "validates input" <|
                    \_ ->
                        Tuple.first result
                            |> Input.errors
                            |> Expect.equal [ IsBlank (Just BlankField) ]
                ]

            -- , describe "and errors are presented in the correct order" []
            ]
        ]


type Field
    = StringField
    | IntField
    | FloatField
    | BoolField
    | PosixField
    | BlankField


stringInput : Input.Input Field
stringInput =
    Input.text
        [ Input.label "Enter your string"
        , Input.identifier StringField
        , Input.name "string-field"
        , Input.value (Value.string "A string")
        ]


intInput : Input.Input Field
intInput =
    Input.text
        [ Input.label "Enter your int"
        , Input.identifier IntField
        , Input.name "int-field"
        , Input.value (Value.int 1)
        ]


boolInput : Input.Input Field
boolInput =
    Input.text
        [ Input.label "Enter your bool"
        , Input.identifier BoolField
        , Input.value (Value.bool True)
        ]


floatInput : Input.Input Field
floatInput =
    Input.text
        [ Input.label "Enter your float"
        , Input.identifier FloatField
        , Input.value (Value.float 1.1)
        ]


posixInput : Input.Input Field
posixInput =
    Input.text
        [ Input.label "Enter your posix"
        , Input.identifier PosixField
        , Input.value (Value.time (Time.millisToPosix 0))
        ]


blankInput : Input.Input Field
blankInput =
    Input.text
        [ Input.label "Enter nothing"
        , Input.required True
        , Input.identifier BlankField
        ]


groupWithName : Input.Input Field
groupWithName =
    Input.group [ Input.name "group" ] [ stringInput, intInput ]


groupWithNoName : Input.Input Field
groupWithNoName =
    Input.group [] [ stringInput, intInput ]


simpleJsonDecoder : Json.Decode.Decoder ( String, Int )
simpleJsonDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "string-field" Json.Decode.string)
        (Json.Decode.field "int-field" Json.Decode.int)


groupWithNameDecoder : Json.Decode.Decoder ( String, Int )
groupWithNameDecoder =
    Json.Decode.field "group" simpleJsonDecoder
