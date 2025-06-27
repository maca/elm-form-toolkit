module ParseTest exposing (suite)

import Expect
import FormToolkit.Error exposing (Error(..))
import FormToolkit.Field as Input
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Json.Decode
import Json.Encode
import Support.ExampleInputs exposing (..)
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Decoding"
        [ describe "succeeds"
            [ test "decoding string" <|
                \_ ->
                    Parse.parse Parse.string stringInput
                        |> Expect.equal (Ok "A string")
            , test "decoding int" <|
                \_ ->
                    Parse.parse Parse.int intInput
                        |> Expect.equal (Ok 1)
            , test "decoding float" <|
                \_ ->
                    Parse.parse Parse.float floatInput
                        |> Expect.equal (Ok 1.1)
            , test "decoding bool" <|
                \_ ->
                    Parse.parse Parse.bool boolInput
                        |> Expect.equal (Ok True)
            , test "decoding posix" <|
                \_ ->
                    Parse.parse Parse.posix posixInput
                        |> Expect.equal (Ok (Time.millisToPosix 0))
            , test "decoding custom value with field with options" <|
                \_ ->
                    Input.select
                        [ Input.value (Value.string "Español")
                        , Input.stringOptions [ "Español", "English", "Deutsch" ]
                        ]
                        |> Parse.parse Parse.string
                        |> Expect.equal (Ok "Español")
            , test "decoding field by id" <|
                \_ ->
                    Input.group [] [ stringInput ]
                        |> Parse.parse
                            (Parse.field StringField Parse.string)
                        |> Expect.equal (Ok "A string")
            ]
        , describe "failure"
            [ describe "on simple decoder"
                [ test "produces error" <|
                    \_ ->
                        Parse.parse Parse.int stringInput
                            |> Expect.equal
                                (Err [ ParseError (Just StringField) ])
                ]
            , describe "on field decoding"
                [ test "produces error" <|
                    \_ ->
                        Input.group [] [ stringInput ]
                            |> Parse.parse (Parse.field StringField Parse.int)
                            |> Expect.equal (Err [ ParseError (Just StringField) ])
                ]
            ]
        , describe "encode json"
            [ test "string" <|
                \_ ->
                    Parse.parse Parse.json stringInput
                        |> Result.withDefault (Json.Encode.string "")
                        |> Json.Decode.decodeValue
                            (Json.Decode.field "string-field" Json.Decode.string)
                        |> Expect.equal (Ok "A string")
            , test "group with no name" <|
                \_ ->
                    Parse.parse Parse.json
                        (Input.group [] [ stringInput, intInput ])
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue simpleJsonDecoder
                        |> Expect.equal (Ok ( "A string", 1 ))
            , test "group name" <|
                \_ ->
                    Parse.parse Parse.json groupWithName
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue groupWithNameDecoder
                        |> Expect.equal (Ok ( "A string", 1 ))

            -- , test "repeatable and group with name" <|
            --     \_ ->
            --         Parse.decode Parse.json
            --             (Input.repeatable [ Input.name "repeatable" ]
            --                 groupWithName
            --                 [ groupWithName
            --                 , groupWithName
            --                 ]
            --             )
            --             |> Result.withDefault Json.Encode.null
            --             |> Json.Parse.decodeValue
            --                 (Json.Decode.field "repeatable"
            --                     (Json.Decode.list groupWithNameDecoder)
            --                 )
            --             |> Expect.equal
            --                 (Ok [ ( "A string", 1 ), ( "A string", 1 ) ])
            -- , test "repeatable and group with noname" <|
            --     \_ ->
            --         Parse.decode Parse.json
            --             (Input.repeatable [ Input.name "repeatable" ]
            --                 groupWithNoName
            --                 [ groupWithNoName, groupWithNoName ]
            --             )
            --             |> Result.withDefault Json.Encode.null
            --             |> Json.Decode.decodeValue
            --                 (Json.Decode.field "repeatable"
            --                     (Json.Decode.list simpleJsonDecoder)
            --                 )
            --             |> Expect.equal
            --                 (Ok [ ( "A string", 1 ), ( "A string", 1 ) ])
            ]
        , describe "validates"
            [ test "presence" <|
                \_ ->
                    Parse.parse (Parse.succeed ()) blankInput
                        |> Expect.equal
                            (Err [ IsBlank (Just BlankField) ])
            , test "nested field not handled" <|
                \_ ->
                    Input.group [] [ blankInput ]
                        |> Parse.parse (Parse.succeed ())
                        |> Expect.equal (Err [ IsBlank (Just BlankField) ])
            , test "errors are presented in correct order" <|
                \_ ->
                    let
                        result =
                            Input.group [] [ stringInput, intInput ]
                                |> Parse.parse
                                    (Parse.succeed (\_ _ -> ())
                                        |> Parse.andMap
                                            (Parse.field StringField Parse.float)
                                        |> Parse.andMap
                                            (Parse.field IntField Parse.float)
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
                    Input.group [] [ stringInput ]
                        |> Parse.parse
                            (Parse.succeed (\_ _ -> ())
                                |> Parse.andMap
                                    (Parse.field StringField Parse.float)
                                |> Parse.andMap
                                    (Parse.field StringField Parse.float)
                            )
                        |> Expect.equal
                            (Err [ ParseError (Just StringField) ])

            -- , describe "and errors are presented in the correct order" []
            -- , test errors are not repeated after multiple updates
            -- , test group decoding yields error (implemented)
            -- , describe "blank string" should be blank
            ]

        -- , describe "format"
        --     [ test "transforms string" <|
        --         \_ ->
        --             let
        --                 { result } =
        --                     stringInput
        --                         |> Interaction.init (Parse.format removeVowels)
        --                         |> fillInput "string-field"
        --                             "the quick fox jumps over the lazy dog"
        --                         |> fillInput "string-field"
        --                             "the quick fox jumps over the lazy dog"
        --             in
        --             Expect.equal result
        --                 (Ok "th qck fx jmps vr th lzy dg")
        --     ]
        ]


simpleJsonDecoder : Json.Decode.Decoder ( String, Int )
simpleJsonDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "string-field" Json.Decode.string)
        (Json.Decode.field "int-field" Json.Decode.int)


groupWithNameDecoder : Json.Decode.Decoder ( String, Int )
groupWithNameDecoder =
    Json.Decode.field "group" simpleJsonDecoder



-- format : (String -> String) -> Parser id val String
-- format func =
--     Parser
--         (\input ->
--             case
--                 Internal.Field.value input
--                     |> Internal.Value.toString
--             of
--                 Just str ->
--                     Success
--                         (Tree.updateValue
--                             (\attrs ->
--                                 { attrs | value = Internal.Value.Text (func str) }
--                             )
--                             input
--                         )
--                         ()
--                 Nothing ->
--                     Success input ()
--         )
--         |> andThen (\() -> string)
-- removeVowels : String -> String
-- removeVowels =
--     String.replace "a" ""
--         >> String.replace "e" ""
--         >> String.replace "i" ""
--         >> String.replace "o" ""
--         >> String.replace "u" ""
