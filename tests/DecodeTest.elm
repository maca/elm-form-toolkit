module DecodeTest exposing (suite)

import Expect
import FormToolkit.Decode as Decode
import FormToolkit.Input as Input exposing (Error(..))
import FormToolkit.Value as Value
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Decoding"
        [ describe "success"
            [ test "decodes string" <|
                \_ ->
                    Decode.decode Decode.string stringInput
                        |> Expect.equal (Ok "A string")
            , test "decodes int" <|
                \_ ->
                    Decode.decode Decode.int intInput
                        |> Expect.equal (Ok 1)
            , test "decodes float" <|
                \_ ->
                    Decode.decode Decode.float floatInput
                        |> Expect.equal (Ok 1.1)
            , test "decodes bool" <|
                \_ ->
                    Decode.decode Decode.bool boolInput
                        |> Expect.equal (Ok True)
            , test "decodes posix" <|
                \_ ->
                    Decode.decode Decode.posix posixInput
                        |> Expect.equal (Ok (Time.millisToPosix 0))
            , test "decodes field by id" <|
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
                            |> Expect.equal (Err [ ParseError ])
                , test "validates input" <|
                    \_ ->
                        Decode.validateAndDecode Decode.int stringInput
                            |> Tuple.first
                            |> Input.errors
                            |> Expect.equal [ ParseError ]
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
                            |> Expect.equal (Err [ ParseError ])
                , test "validates input" <|
                    \_ ->
                        Tuple.first result
                            |> Input.errors
                            |> Expect.equal [ ParseError ]
                ]
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
                            |> Expect.equal (Err [ IsBlank ])
                , test "validates input" <|
                    \_ ->
                        Tuple.first result
                            |> Input.errors
                            |> Expect.equal [ IsBlank ]
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
                            |> Expect.equal (Err [ IsBlank ])
                , test "validates input" <|
                    \_ ->
                        Tuple.first result
                            |> Input.errors
                            |> Expect.equal [ IsBlank ]
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
        , Input.value (Value.string "A string")
        ]


intInput : Input.Input Field
intInput =
    Input.text
        [ Input.label "Enter your int"
        , Input.identifier IntField
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
