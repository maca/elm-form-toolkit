module DecodeTest exposing (suite)

import Expect
import FormToolkit.Decode as Decode exposing (Error(..))
import FormToolkit.Input as Input
import FormToolkit.Value as Value
import Json.Decode
import Json.Encode
import Support.Interaction as Interaction exposing (..)
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
                ( updatedInput, result ) =
                    Decode.validateAndDecode (Decode.succeed ()) blankInput
              in
              describe "presence when decoding succeeds"
                [ test "produces error" <|
                    \_ ->
                        result
                            |> Expect.equal
                                (Err [ IsBlank (Just BlankField) ])
                , test "validates input" <|
                    \_ ->
                        updatedInput
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
            , test "errors are presented in correct order" <|
                \_ ->
                    let
                        ( _, result ) =
                            Input.group [] [ stringInput, intInput ]
                                |> Decode.validateAndDecode
                                    (Decode.succeed (\_ _ -> ())
                                        |> Decode.andMap
                                            (Decode.field StringField Decode.float)
                                        |> Decode.andMap
                                            (Decode.field IntField Decode.float)
                                    )
                    in
                    result
                        |> Expect.equal
                            (Err
                                [ ParseError (Just StringField)
                                , ParseError (Just IntField)
                                ]
                            )
            , test "errors are not repeated" <|
                \_ ->
                    let
                        ( updatedInput, result ) =
                            Input.group [] [ stringInput ]
                                |> Decode.validateAndDecode
                                    (Decode.succeed (\_ _ -> ())
                                        |> Decode.andMap
                                            (Decode.field StringField Decode.float)
                                        |> Decode.andMap
                                            (Decode.field StringField Decode.float)
                                    )
                    in
                    Expect.all
                        [ \_ ->
                            result
                                |> Expect.equal
                                    (Err [ ParseError (Just StringField) ])
                        , \_ ->
                            Input.errors updatedInput
                                |> Expect.equal
                                    [ ParseError (Just StringField) ]
                        ]
                        ()

            -- , describe "and errors are presented in the correct order" []
            -- , test errors are not repeated after multiple updates
            -- , test group decoding yields error (implemented)
            -- , describe "blank string" should be blank
            ]
        , describe "format"
            [ test "transforms string" <|
                \_ ->
                    let
                        { result } =
                            stringInput
                                |> Interaction.init (Decode.format removeVowels)
                                |> fillInput "string-field"
                                    "the quick fox jumps over the lazy dog"
                                |> fillInput "string-field"
                                    "the quick fox jumps over the lazy dog"
                    in
                    Expect.equal result
                        (Ok "th qck fx jmps vr th lzy dg")
            ]
        ]


type Field
    = StringField
    | IntField
    | FloatField
    | BoolField
    | PosixField
    | BlankField


stringInput : Input.Input Field val
stringInput =
    Input.text
        [ Input.label "Enter your string"
        , Input.identifier StringField
        , Input.name "string-field"
        , Input.value (Value.string "A string")
        ]


intInput : Input.Input Field val
intInput =
    Input.text
        [ Input.label "Enter your int"
        , Input.identifier IntField
        , Input.name "int-field"
        , Input.value (Value.int 1)
        ]


boolInput : Input.Input Field val
boolInput =
    Input.text
        [ Input.label "Enter your bool"
        , Input.identifier BoolField
        , Input.value (Value.bool True)
        ]


floatInput : Input.Input Field val
floatInput =
    Input.text
        [ Input.label "Enter your float"
        , Input.identifier FloatField
        , Input.value (Value.float 1.1)
        ]


posixInput : Input.Input Field val
posixInput =
    Input.text
        [ Input.label "Enter your posix"
        , Input.identifier PosixField
        , Input.value (Value.time (Time.millisToPosix 0))
        ]


blankInput : Input.Input Field val
blankInput =
    Input.text
        [ Input.label "Enter nothing"
        , Input.required True
        , Input.identifier BlankField
        ]


groupWithName : Input.Input Field val
groupWithName =
    Input.group [ Input.name "group" ] [ stringInput, intInput ]


groupWithNoName : Input.Input Field val
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


removeVowels : String -> String
removeVowels =
    String.replace "a" ""
        >> String.replace "e" ""
        >> String.replace "i" ""
        >> String.replace "o" ""
        >> String.replace "u" ""
