module ParseTest exposing (suite)

import Expect
import FormToolkit.Error as Error exposing (Error(..))
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html.Attributes as Attrs
import Json.Decode
import Json.Encode
import Support.ExampleInputs exposing (..)
import Support.Interaction as Interaction
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute)
import Time


suite : Test
suite =
    describe "Decoding"
        [ describe "succeeds"
            [ test "decoding string" <|
                \_ ->
                    Parse.parse Parse.string stringInput
                        |> Tuple.second
                        |> Expect.equal (Ok "A string")
            , test "decoding int" <|
                \_ ->
                    Parse.parse Parse.int intInput
                        |> Tuple.second
                        |> Expect.equal (Ok 1)
            , test "decoding float" <|
                \_ ->
                    Parse.parse Parse.float floatInput
                        |> Tuple.second
                        |> Expect.equal (Ok 1.1)
            , test "decoding bool" <|
                \_ ->
                    Parse.parse Parse.bool boolInput
                        |> Tuple.second
                        |> Expect.equal (Ok True)
            , test "decoding posix" <|
                \_ ->
                    Parse.parse Parse.posix posixInput
                        |> Tuple.second
                        |> Expect.equal (Ok (Time.millisToPosix 0))
            , test "decoding custom value with field with options" <|
                \_ ->
                    Field.select
                        [ Field.value (Value.string "Español")
                        , Field.stringOptions [ "Español", "English", "Deutsch" ]
                        ]
                        |> Parse.parse Parse.string
                        |> Tuple.second
                        |> Expect.equal (Ok "Español")
            , test "decoding field by id" <|
                \_ ->
                    Field.group [] [ stringInput ]
                        |> Parse.parse
                            (Parse.field StringField Parse.string)
                        |> Tuple.second
                        |> Expect.equal (Ok "A string")
            ]
        , describe "failure"
            [ describe "on simple decoder"
                [ test "produces error" <|
                    \_ ->
                        Parse.parse Parse.int stringInput
                            |> Tuple.second
                            |> Expect.equal
                                (Err [ ParseError (Just StringField) ])
                ]
            , describe "on field decoding"
                [ test "produces error" <|
                    \_ ->
                        Field.group [] [ stringInput ]
                            |> Parse.parse (Parse.field StringField Parse.int)
                            |> Tuple.second
                            |> Expect.equal (Err [ ParseError (Just StringField) ])
                ]
            ]
        , describe "encode json"
            [ test "string" <|
                \_ ->
                    Parse.parse Parse.json stringInput
                        |> Tuple.second
                        |> Result.withDefault (Json.Encode.string "")
                        |> Json.Decode.decodeValue
                            (Json.Decode.field "string-field" Json.Decode.string)
                        |> Expect.equal (Ok "A string")
            , test "group with no name" <|
                \_ ->
                    Parse.parse Parse.json
                        (Field.group [] [ stringInput, intInput ])
                        |> Tuple.second
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue simpleJsonDecoder
                        |> Expect.equal (Ok ( "A string", 1 ))
            , test "group name" <|
                \_ ->
                    Parse.parse Parse.json groupWithName
                        |> Tuple.second
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue groupWithNameDecoder
                        |> Expect.equal (Ok ( "A string", 1 ))
            , test "repeatable and group with name" <|
                \_ ->
                    Parse.parse Parse.json
                        (Field.repeatable [ Field.name "repeatable" ]
                            groupWithName
                            []
                        )
                        |> Tuple.second
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue
                            (Json.Decode.field "repeatable"
                                (Json.Decode.list groupWithNameDecoder)
                            )
                        |> Expect.equal
                            (Ok [ ( "A string", 1 ) ])
            , test "repeatable and group with noname" <|
                \_ ->
                    Parse.parse Parse.json
                        (Field.repeatable [ Field.name "repeatable" ]
                            (Field.group [] [ stringInput, intInput ])
                            []
                        )
                        |> Tuple.second
                        |> Result.withDefault Json.Encode.null
                        |> Json.Decode.decodeValue
                            (Json.Decode.field "repeatable"
                                (Json.Decode.list simpleJsonDecoder)
                            )
                        |> Expect.equal
                            (Ok [ ( "A string", 1 ) ])
            ]
        , describe "validates"
            [ test "presence" <|
                \_ ->
                    Parse.parse (Parse.succeed ()) blankInput
                        |> Tuple.second
                        |> Expect.equal
                            (Err [ IsBlank (Just BlankField) ])
            , test "nested field not handled" <|
                \_ ->
                    Field.group [] [ blankInput ]
                        |> Parse.parse (Parse.succeed ())
                        |> Tuple.second
                        |> Expect.equal (Err [ IsBlank (Just BlankField) ])
            , test "errors are presented in correct order" <|
                \_ ->
                    let
                        result =
                            Field.group [] [ stringInput, intInput ]
                                |> Parse.parse
                                    (Parse.succeed (\_ _ -> ())
                                        |> Parse.andMap
                                            (Parse.field StringField Parse.float)
                                        |> Parse.andMap
                                            (Parse.field IntField Parse.float)
                                    )
                    in
                    result
                        |> Tuple.second
                        |> Expect.equal
                            (Err
                                [ ParseError (Just StringField)
                                , ParseError (Just IntField)
                                ]
                            )
            , test "errors are not repeated" <|
                \_ ->
                    Field.group [] [ stringInput ]
                        |> Parse.parse
                            (Parse.succeed (\_ _ -> ())
                                |> Parse.andMap
                                    (Parse.field StringField Parse.float)
                                |> Parse.andMap
                                    (Parse.field StringField Parse.float)
                            )
                        |> Tuple.second
                        |> Expect.equal
                            (Err [ ParseError (Just StringField) ])
            , test "email validation only applies to email fields" <|
                \_ ->
                    let
                        invalidEmail =
                            "not-an-email"

                        textField =
                            Field.text
                                [ Field.identifier StringField
                                , Field.value (Value.string invalidEmail)
                                ]

                        emailField =
                            Field.email
                                [ Field.identifier StringField
                                , Field.value (Value.string invalidEmail)
                                ]
                    in
                    Expect.all
                        [ \_ ->
                            -- Text field should NOT validate email format
                            Parse.parse Parse.string textField
                                |> Tuple.second
                                |> Expect.equal (Ok invalidEmail)
                        , \_ ->
                            -- Email field SHOULD validate email format
                            Parse.parse Parse.string emailField
                                |> Tuple.second
                                |> Expect.equal (Err [ EmailInvalid (Just StringField) ])
                        ]
                        ()
            , test "valid email passes validation" <|
                \_ ->
                    let
                        validEmail =
                            "test@example.com"

                        emailField =
                            Field.email
                                [ Field.identifier StringField
                                , Field.value (Value.string validEmail)
                                ]
                    in
                    Parse.parse Parse.string emailField
                        |> Tuple.second
                        |> Expect.equal (Ok validEmail)
            , test "email validation edge cases" <|
                \_ ->
                    let
                        testEmail email shouldPass =
                            let
                                emailField =
                                    Field.email
                                        [ Field.identifier StringField
                                        , Field.value (Value.string email)
                                        ]

                                result =
                                    Parse.parse Parse.string emailField
                                        |> Tuple.second
                            in
                            if shouldPass then
                                result |> Expect.equal (Ok email)

                            else
                                case result of
                                    Err [ EmailInvalid (Just StringField) ] ->
                                        Expect.pass

                                    _ ->
                                        Expect.fail ("Expected EmailInvalid for invalid email: " ++ email)
                    in
                    Expect.all
                        [ \_ -> testEmail "valid@example.com" True
                        , \_ -> testEmail "user.name@domain.com" True
                        , \_ -> testEmail "user+tag@example.co.uk" True
                        , \_ -> testEmail "invalid-email" False
                        , \_ -> testEmail "@domain.com" False
                        , \_ -> testEmail "user@" False
                        , \_ -> testEmail "user@example" False
                        , \_ -> testEmail "user@@domain.com" False
                        ]
                        ()
            , test "empty email field validation" <|
                \_ ->
                    let
                        emptyEmailField =
                            Field.email
                                [ Field.identifier StringField
                                , Field.required True
                                ]
                    in
                    -- Empty required email field should fail with both ParseError and IsBlank
                    Parse.parse Parse.string emptyEmailField
                        |> Tuple.second
                        |> Expect.equal (Err [ ParseError (Just StringField), IsBlank (Just StringField) ])
            , test "EmailInvalid error has correct English translation" <|
                \_ ->
                    EmailInvalid (Just StringField)
                        |> Error.toEnglish
                        |> Expect.equal "Please enter a valid email address"

            -- , describe "and errors are presented in the correct order" []
            -- , test errors are not repeated after multiple updates
            -- , test group decoding yields error (implemented)
            -- , describe "blank string" should be blank
            ]
        , let
            original =
                "the quick fox jumps over the lazy dog"

            expected =
                "th qck fx jmps vr th lzy dg"
          in
          describe "update input"
            [ test "updates a string without affecting parse results" <|
                \_ ->
                    let
                        interaction =
                            stringInput
                                |> Interaction.init
                                    (Parse.string
                                        |> Parse.andUpdate
                                            (\field str ->
                                                { field = Field.updateStringValue (removeVowels str) field
                                                , parseResult = Ok str
                                                }
                                            )
                                    )
                                |> Interaction.fillInput "string-field" original
                    in
                    interaction
                        |> Expect.all
                            [ .field
                                >> Field.toHtml (always never)
                                >> Query.fromHtml
                                >> Query.has [ attribute (Attrs.attribute "value" expected) ]
                            , .result >> Expect.equal (Ok original)
                            ]
            , test "updates a string affecting parse results" <|
                \_ ->
                    let
                        interaction =
                            stringInput
                                |> Interaction.init
                                    (Parse.string
                                        |> Parse.andUpdate
                                            (\field str ->
                                                { field = Field.updateStringValue (removeVowels str) field
                                                , parseResult = Ok (removeVowels str)
                                                }
                                            )
                                    )
                                |> Interaction.fillInput "string-field" original
                    in
                    interaction
                        |> Expect.all
                            [ .field
                                >> Field.toHtml (always never)
                                >> Query.fromHtml
                                >> Query.has [ attribute (Attrs.attribute "value" expected) ]
                            , .result >> Expect.equal (Ok expected)
                            ]
            ]
        ]


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
