module FormToolkit.Parse exposing
    ( Parser
    , parse
    , field
    , string, int, float, bool, posix, maybe, list
    , value, customValue, json
    , succeed, fail, custom
    , map, map2, map3, map4, map5, map6, map7, map8
    , andThen, andMap
    )

{-| Map the values of an input or group of inputs to any shape you want, if you
know `Json.Decode` you know how to use this module ;)

@docs Parser


# Parsing

@docs parse
@docs field


# Values

@docs string, int, float, bool, posix, maybe, list
@docs value, customValue, json
@docs succeed, fail, custom


# Maps, combinators and pipeline style decoding

@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap

-}

import FormToolkit.Error exposing (Error)
import FormToolkit.Field exposing (Field(..))
import FormToolkit.Value as Value
import Internal.Parse
import Json.Decode
import Time


{-| A parser that takes a tree of input data and returns a parsed result or an
error if the decoding fails.
-}
type Parser id val a
    = Parser (Internal.Parse.Parser id val a)


{-| Parse for a field with the given identifier using a provided parser.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    type Fields
        = FirstName
        | LastName

    form : Field Fields val
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

    form |> parse (field FirstName string)
    --> Ok "Brian"

-}
field : id -> Parser id val a -> Parser id val a
field id (Parser parser) =
    Parser (Internal.Parse.field id parser)


{-| Parses the input value as a `String`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "A string") ]
        |> parse string
        --> Ok "A string"

-}
string : Parser id val String
string =
    parseValue Value.toString


{-| Parses the input value as an `Int`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.int 10) ]
        |> parse int
        --> Ok 10

-}
int : Parser id val Int
int =
    parseValue Value.toInt


{-| Parses the input value as a `Float`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.float 10.5) ]
        |> parse float
        --> Ok 10.5

-}
float : Parser id val Float
float =
    parseValue Value.toFloat


{-| Parses the input value as a `Bool`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.bool True) ]
        |> parse bool
        --> Ok True

-}
bool : Parser id val Bool
bool =
    parseValue Value.toBool


{-| Parses the input value as a
[Time.Posix](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix).
-}
posix : Parser id val Time.Posix
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
maybe : Parser id val a -> Parser id val (Maybe a)
maybe (Parser parser) =
    Parser (Internal.Parse.maybe parser)


{-| Parses a list of inputs using the given parser.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.repeatable [ ]
        (Field.text [ ])
        [ Field.updateAttribute
            (Field.value (Value.string "mango") )
        , Field.updateAttribute
            (Field.value (Value.string "banana") )
        ]
        |> parse (list string)
        --> Ok [ "mango", "banana" ]

-}
list : Parser id val a -> Parser id val (List a)
list (Parser parser) =
    Parser (Internal.Parse.list parser)


{-| Returns the raw value of the input without any decoding.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "A string") ]
        |> parse value
        --> Ok (Value.string "A string")

-}
value : Parser id val (Value.Value val)
value =
    parseValue Just


{-| Parses the custom value of an input.

Typically used for `select` or `radio` inputs with options of custom value, but
also for autocompleatable text inputs where the inputted text corresponds to an
option text.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    type Lang
        = ES
        | EN
        | DE

    langSelect : Field id Lang
    langSelect =
        Field.select
            [ Field.label "Language"
            , Field.value (Value.custom ES)
            , Field.options
                [ ( "Español", Value.custom ES )
                , ( "English", Value.custom EN )
                , ( "Deutsch", Value.custom DE )
                ]
            ]

    autocomplete : Field id Lang
    autocomplete =
        Field.text
            [ Field.value (Value.string "English")
            , Field.options
                [ ( "Español", Value.custom ES )
                , ( "English", Value.custom EN )
                , ( "Deutsch", Value.custom DE )
                ]
            ]

    langSelect |> parse customValue
    --> Ok ES

    autocomplete |> parse customValue
    --> Ok EN

-}
customValue : Parser id val val
customValue =
    parseValue Value.toCustom


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
            [ Field.updateAttribute
                (Field.value (Value.string "mango") )
            , Field.updateAttribute
                (Field.value (Value.string "banana") )
            ]
        ]
        |> parse json
        |> Result.map (Json.Encode.encode 0)
        --> Ok "{\"first-name\":\"Brian\",\"last-name\":\"Eno\",\"fruits\":[{\"fruit\":\"mango\"},{\"fruit\":\"banana\"}]}"

-}
json : Parser id val Json.Decode.Value
json =
    Parser Internal.Parse.json


{-| A parser that always succeeds with the given value, use for buiding
decoding pipelines with [andMap](#andMap), or to chain parsers with
[andThen](#andThen).

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    type Special
        = SpecialValue

    specialParse : Parser id val Special
    specialParse =
        string
            |> andThen
                (\strValue ->
                    if strValue == "special" then
                        succeed SpecialValue

                    else
                        fail "Nothing special"
                )


    Field.text [ Field.value (Value.string "special") ] |> parse specialParse
    --> Ok SpecialValue

-}
succeed : a -> Parser id val a
succeed a =
    custom (always (Ok a))


{-| A parser that always fails with a custom error.
-}
fail : String -> Parser id val a
fail err =
    custom (always (Err err))


{-| -}
custom : (Value.Value val -> Result String a) -> Parser id val a
custom =
    Internal.Parse.custom >> Parser


parseValue : (Value.Value val -> Maybe a) -> Parser id val a
parseValue func =
    Parser (Internal.Parse.parseValue func)


{-| Chains together parsers that depend on previous decoding results.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    wordsParser : Parser id val (List String)
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
andThen : (a -> Parser id val b) -> Parser id val a -> Parser id val b
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

    form : Field Fields val
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

    personParse : Parser Fields val Person
    personParse =
        succeed Person
            |> andMap (field FirstName string)
            |> andMap (field LastName string)
            |> andMap (field Age int)


    form |> parse personParse
    --> Ok { firstName = "Penny", lastName = "Rimbaud", age = 81 }

-}
andMap : Parser id val a -> Parser id val (a -> b) -> Parser id val b
andMap a b =
    map2 (|>) a b


{-| Transforms the result of a parser using a function.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "a string") ]
        |> parse (map String.toUpper string)
        --> Ok "A STRING"

-}
map : (a -> b) -> Parser id val a -> Parser id val b
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
map2 : (a -> b -> c) -> Parser id val a -> Parser id val b -> Parser id val c
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

    personParse : Parser String val Person
    personParse =
        map3 Person
            (field "FirstName" string)
            (field "LastName" string)
            (field "Age" int)

    form : Field String val
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

    form |> parse personParse
    --> Ok { firstName = "Penny", lastName = "Rimbaud", age = 81 }

-}
map3 :
    (a -> b -> c -> out)
    -> Parser id val a
    -> Parser id val b
    -> Parser id val c
    -> Parser id val out
map3 func a b c =
    map2 func a b |> andMap c


{-| -}
map4 :
    (a -> b -> c -> d -> out)
    -> Parser id val a
    -> Parser id val b
    -> Parser id val c
    -> Parser id val d
    -> Parser id val out
map4 func a b c d =
    map3 func a b c |> andMap d


{-| -}
map5 :
    (a -> b -> c -> d -> e -> out)
    -> Parser id val a
    -> Parser id val b
    -> Parser id val c
    -> Parser id val d
    -> Parser id val e
    -> Parser id val out
map5 func a b c d e =
    map4 func a b c d |> andMap e


{-| -}
map6 :
    (a -> b -> c -> d -> e -> f -> out)
    -> Parser id val a
    -> Parser id val b
    -> Parser id val c
    -> Parser id val d
    -> Parser id val e
    -> Parser id val f
    -> Parser id val out
map6 func a b c d e f =
    map5 func a b c d e |> andMap f


{-| -}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> out)
    -> Parser id val a
    -> Parser id val b
    -> Parser id val c
    -> Parser id val d
    -> Parser id val e
    -> Parser id val f
    -> Parser id val g
    -> Parser id val out
map7 func a b c d e f g =
    map6 func a b c d e f |> andMap g


{-| -}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> out)
    -> Parser id val a
    -> Parser id val b
    -> Parser id val c
    -> Parser id val d
    -> Parser id val e
    -> Parser id val f
    -> Parser id val g
    -> Parser id val h
    -> Parser id val out
map8 func a b c d e f g h =
    map7 func a b c d e f g |> andMap h


{-| Parses an input using the given parser without applying field validations
(required, min, max...).

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    Field.text
        [ Field.value (Value.string "A string")
        , Field.required True
        ]
        |> parse string
    --> Ok "A string"

-}
parse : Parser id val a -> Field id val -> Result (List (Error id val)) a
parse (Parser parser) (Field input) =
    Internal.Parse.parse parser input


unwrap : Parser id val b -> Internal.Parse.Parser id val b
unwrap (Parser parser) =
    parser
