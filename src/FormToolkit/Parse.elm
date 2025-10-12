module FormToolkit.Parse exposing
    ( Parser, parse, parseUpdate
    , field
    , string, int, float, bool, posix, maybe, list
    , formattedString
    , value, json
    , succeed, fail
    , map, map2, map3, map4, map5, map6, map7, map8
    , andThen, andMap
    , andUpdate
    )

{-| Map the values of an input or group of inputs to any shape you want, if you
know `Json.Decode` you know how to use this module ;)

@docs Parser, parse, parseUpdate


# Traversing and parsing

@docs field
@docs string, int, float, bool, posix, maybe, list
@docs formattedString
@docs value, json
@docs succeed, fail


# Maps, combinators and pipeline style decoding

@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap


# Input update and custom validations

@docs andUpdate

-}

import FormToolkit.Error as Error exposing (Error)
import FormToolkit.Field as Field exposing (Field(..), Msg)
import FormToolkit.Value as Value
import Internal.Field
import Internal.Parse
import Internal.Utils as Utils
import Json.Decode
import Time


{-| A parser that takes a tree of input data and returns a parsed result or an
error if the decoding fails.
-}
type Parser id a
    = Parser (Internal.Parse.Parser id a)


{-| Parse for a field with the given identifier using a provided parser.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    type Fields
        = FirstName
        | LastName

    form : Field Fields
    form =
        Field.group []
            [ Field.text
                [ Field.label "First name"
                , Field.identifier FirstName
                , Field.value (Value.string "Brian")
                ]
            , Field.text
                [ Field.label "Last name"
                , Field.identifier LastName
                , Field.value (Value.string "Eno")
                ]
            ]

    form
        |> parse (field FirstName string)
    --> Ok "Brian"

-}
field : id -> Parser id a -> Parser id a
field id (Parser parser) =
    Parser (Internal.Parse.field id parser)


{-| Parses the input value as a `String`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "A string") ]
        |> parse string
        --> Ok "A string"

-}
string : Parser id String
string =
    parseValue Value.toString


{-| Parses the input value as an `Int`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.int 10) ]
        |> parse int
        --> Ok 10

-}
int : Parser id Int
int =
    parseValue Value.toInt


{-| Parses the input value as a `Float`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.float 10.5) ]
        |> parse float
        --> Ok 10.5

-}
float : Parser id Float
float =
    parseValue Value.toFloat


{-| Parses the input value as a `Bool`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.bool True) ]
        |> parse bool
        --> Ok True

-}
bool : Parser id Bool
bool =
    parseValue Value.toBool


{-| Parses the input value as a
[Time.Posix](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix).
-}
posix : Parser id Time.Posix
posix =
    parseValue Value.toPosix


{-| Allows dealing with blank values without producing an error.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "A string") ]
        |> parse (maybe string)
        --> Ok (Just "A string")

    Field.text []
        |> parse (maybe string)
        --> Ok Nothing

-}
maybe : Parser id a -> Parser id (Maybe a)
maybe (Parser parser) =
    Parser (Internal.Parse.maybe parser)


{-| Parses a list of inputs using the given parser.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.repeatable [ ]
        (Field.text [ ])
        [ Ok << Field.updateValue (Value.string "mango")
        , Ok << Field.updateValue (Value.string "banana")
        ]
        |> parse (list string)
        --> Ok [ "mango", "banana" ]

-}
list : Parser id a -> Parser id (List a)
list (Parser parser) =
    Parser (Internal.Parse.list parser)


{-| Returns the raw value of the input without any decoding.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "A string") ]
        |> parse value
        --> Ok (Value.string "A string")

-}
value : Parser id Value.Value
value =
    parseValue Just


{-| Converts the entire input tree into a JSON
[Value](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode#Value).
Field `name` property will be used as the key, if an input name is not present
the parser will fail.

Useful if you just want to forward the form values to a backend.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value
    import Json.Encode

    Field.group [ ]
        [ Field.text
            [ Field.label "First name"
            , Field.name "first-name"
            , Field.value (Value.string "Brian")
            ]
        , Field.text
            [ Field.label "Last name"
            , Field.name "last-name"
            , Field.value (Value.string "Eno")
            ]
        , Field.repeatable [ Field.name "fruits" ]
            (Field.text [ Field.name "fruit" ])
            [ Ok << Field.updateValue (Value.string "mango")
            , Ok << Field.updateValue (Value.string "banana")
            ]
        ]
        |> parse json
        |> Result.map (Json.Encode.encode 0)
        --> Ok "{\"first-name\":\"Brian\",\"last-name\":\"Eno\",\"fruits\":[{\"fruit\":\"mango\"},{\"fruit\":\"banana\"}]}"

-}
json : Parser id Json.Decode.Value
json =
    Parser Internal.Parse.json


{-| A parser that always succeeds with the given value, use for buiding
decoding pipelines with [andMap](#andMap), or to chain parsers with
[andThen](#andThen).

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    type Special
        = SpecialValue

    specialParse : Parser id Special
    specialParse =
        string
            |> andThen
                (\strValue ->
                    if strValue == "special" then
                        succeed SpecialValue

                    else
                        fail "Nothing special"
                )


    Field.text [ Field.value (Value.string "special") ]
        |> parse specialParse
        --> Ok SpecialValue

-}
succeed : a -> Parser id a
succeed a =
    custom (always (Ok a))


{-| A parser that always fails with a custom error.
-}
fail : String -> Parser id a
fail err =
    custom (always (Err err))


{-| This function can be used for performing custom validations, for
formatting the input value, and for mapping the result of parsing the field.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    removeVowels : String -> String
    removeVowels =
        String.replace "a" ""
            >> String.replace "e" ""
            >> String.replace "i" ""
            >> String.replace "o" ""
            >> String.replace "u" ""


    Field.text [ Field.value (Value.string "the quick fox jumps over the lazy dog") ]
        |> parse
            (string
                |> andUpdate
                    (\field str ->
                        { field = Field.updateStringValue (removeVowels str) field
                        , parser = succeed (removeVowels str)
                        }
                    )
            )
        --> Ok "th qck fx jmps vr th lzy dg"
        -- And input field displays: "th qck fx jmps vr th lzy dg"

-}
andUpdate :
    (Field id
     -> a
     ->
        { field : Field id
        , parser : Parser id b
        }
    )
    -> Parser id a
    -> Parser id b
andUpdate func (Parser parser) =
    let
        updateFunc pristineField a =
            let
                result =
                    func (Field pristineField) a

                (Field modifiedField) =
                    result.field

                (Parser newParser) =
                    result.parser
            in
            { field = modifiedField, parser = newParser }
    in
    Parser (Internal.Parse.andUpdate updateFunc parser)


{-| -}
custom : (Value.Value -> Result String a) -> Parser id a
custom =
    Internal.Parse.custom >> Parser


parseValue : (Value.Value -> Maybe a) -> Parser id a
parseValue func =
    Parser (Internal.Parse.parseValue func)


{-| Chains together parsers that depend on previous decoding results.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    wordsParser : Parser id (List String)
    wordsParser =
        string
            |> andThen
                (\strValue ->
                    case String.words strValue of
                        [] ->
                            fail "No words"

                        words ->
                            succeed words
                )


    Field.text [ Field.value (Value.string "red green blue") ]
        |> parse wordsParser
        --> Ok (["red", "green", "blue"])

-}
andThen : (a -> Parser id b) -> Parser id a -> Parser id b
andThen func (Parser parser) =
    Parser (Internal.Parse.andThen (func >> unwrap) parser)


{-| Incrementally apply parsers in a pipeline fashion.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    type alias Person =
        { firstName : String
        , lastName : String
        , age : Int
        }

    type Fields
        = FirstName
        | LastName
        | Age

    form : Field Fields
    form =
        Field.group []
            [ Field.text
                [ Field.identifier FirstName
                , Field.value (Value.string "Penny")
                ]
            , Field.text
                [ Field.identifier LastName
                , Field.value (Value.string "Rimbaud")
                ]
            , Field.int
                [ Field.identifier Age
                , Field.value (Value.int 81)
                ]
            ]

    personParse : Parser Fields Person
    personParse =
        succeed Person
            |> andMap (field FirstName string)
            |> andMap (field LastName string)
            |> andMap (field Age int)


    form
        |> parse personParse
    --> Ok { firstName = "Penny", lastName = "Rimbaud", age = 81 }

-}
andMap : Parser id a -> Parser id (a -> b) -> Parser id b
andMap a b =
    map2 (|>) a b


{-| Transforms the result of a parser using a function.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "a string") ]
        |> parse (map String.toUpper string)
        --> Ok "A STRING"

-}
map : (a -> b) -> Parser id a -> Parser id b
map func (Parser parser) =
    Parser (Internal.Parse.map func parser)


{-| Combines two parsers using a function.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.group []
        [ Field.text
            [ Field.identifier "FirstName"
            , Field.value (Value.string "Iris")
            ]
        , Field.text
            [ Field.identifier "LastName"
            , Field.value (Value.string "Hefets")
            ]
        ]
        |> parse
            (map2 Tuple.pair
                (field "FirstName" string)
                (field "LastName" string)
            )
        --> Ok ( "Iris", "Hefets" )

-}
map2 : (a -> b -> c) -> Parser id a -> Parser id b -> Parser id c
map2 func (Parser a) (Parser b) =
    Parser (Internal.Parse.map2 func a b)


{-| Combines three parsers using a function.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    type alias Person =
        { firstName : String
        , lastName : String
        , age : Int
        }

    personParse : Parser String Person
    personParse =
        map3 Person
            (field "FirstName" string)
            (field "LastName" string)
            (field "Age" int)

    form : Field String
    form =
        Field.group []
            [ Field.text
                [ Field.identifier "FirstName"
                , Field.value (Value.string "Penny")
                ]
            , Field.text
                [ Field.identifier "LastName"
                , Field.value (Value.string "Rimbaud")
                ]
            , Field.int
                [ Field.identifier "Age"
                , Field.value (Value.int 81)
                ]
            ]

    form
        |> parse personParse
    --> Ok { firstName = "Penny", lastName = "Rimbaud", age = 81 }

-}
map3 :
    (a -> b -> c -> out)
    -> Parser id a
    -> Parser id b
    -> Parser id c
    -> Parser id out
map3 func a b c =
    map2 func a b |> andMap c


{-| -}
map4 :
    (a -> b -> c -> d -> out)
    -> Parser id a
    -> Parser id b
    -> Parser id c
    -> Parser id d
    -> Parser id out
map4 func a b c d =
    map3 func a b c |> andMap d


{-| -}
map5 :
    (a -> b -> c -> d -> e -> out)
    -> Parser id a
    -> Parser id b
    -> Parser id c
    -> Parser id d
    -> Parser id e
    -> Parser id out
map5 func a b c d e =
    map4 func a b c d |> andMap e


{-| -}
map6 :
    (a -> b -> c -> d -> e -> f -> out)
    -> Parser id a
    -> Parser id b
    -> Parser id c
    -> Parser id d
    -> Parser id e
    -> Parser id f
    -> Parser id out
map6 func a b c d e f =
    map5 func a b c d e |> andMap f


{-| -}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> out)
    -> Parser id a
    -> Parser id b
    -> Parser id c
    -> Parser id d
    -> Parser id e
    -> Parser id f
    -> Parser id g
    -> Parser id out
map7 func a b c d e f g =
    map6 func a b c d e f |> andMap g


{-| -}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> out)
    -> Parser id a
    -> Parser id b
    -> Parser id c
    -> Parser id d
    -> Parser id e
    -> Parser id f
    -> Parser id g
    -> Parser id h
    -> Parser id out
map8 func a b c d e f g h =
    map7 func a b c d e f g |> andMap h


{-| Parses an input and updates the tree- in case the parser
produces validation errors or it updates the input value.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    Field.text
        [ Field.value (Value.string "A string")
        , Field.required True
        ]
        |> parse string
    --> Ok "A string"

-}
parse : Parser id a -> Field id -> Result (List (Error id)) a
parse (Parser parser) (Field input) =
    Internal.Parse.parse parser input |> Tuple.second


{-| Updates an input with a message and parses it.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    Field.text
        [ Field.value (Value.string "A string")
        , Field.required True
        ]
        |> parse string
    --> Ok "A string"

-}
parseUpdate : Parser id a -> Msg id -> Field id -> ( Field id, Result (List (Error id)) a )
parseUpdate (Parser parser) (Field.Msg msg) (Field input) =
    Internal.Field.update msg input
        |> Internal.Parse.parse parser
        |> Tuple.mapFirst Field


unwrap : Parser id b -> Internal.Parse.Parser id b
unwrap (Parser parser) =
    parser


{-| Format a string parser with a mask pattern, updating the field's display value and cursor position.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "1234567890123456") ]
        |> parse (formattedString "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}")
        --> Ok "1234 5678 9012 3456"
        -- And input field displays: "1234 5678 9012 3456"

-}
formattedString : String -> Parser id String
formattedString mask =
    string
        |> andUpdate
            (\currentField rawInput ->
                let
                    { selectionStart } =
                        Field.toProperties currentField

                    { formatted, cursorPosition, maskConsumed } =
                        Utils.formatMask
                            { mask = mask
                            , input = rawInput
                            , cursorPosition = selectionStart
                            }
                in
                { field =
                    currentField
                        |> Field.updateStringValue formatted
                        |> Field.updateAttribute (Field.selectionStart cursorPosition)
                        |> Field.updateAttribute (Field.selectionEnd cursorPosition)
                , parser =
                    if maskConsumed then
                        succeed formatted

                    else
                        Parser
                            (\node ->
                                Internal.Parse.failure node
                                    (Error.PatternError
                                        (Internal.Field.identifier node)
                                    )
                            )
                }
            )
