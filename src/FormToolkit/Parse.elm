module FormToolkit.Parse exposing
    ( Parser, parse, parseUpdate, parseValidate
    , field
    , string, int, float, bool, posix, maybe, list, oneOf
    , formattedString, email
    , value, json
    , succeed, fail
    , map, map2, map3, map4, map5, map6, map7, map8
    , andThen, andMap
    , andUpdate
    )

{-| Map the values of an input or group of inputs to any shape you want, if you
know `Json.Decode` you know how to use this module ;)

@docs Parser, parse, parseUpdate, parseValidate


# Traversing and parsing

@docs field
@docs string, int, float, bool, posix, maybe, list, oneOf
@docs formattedString, email
@docs value, json
@docs succeed, fail


# Maps, combinators and pipeline style decoding

@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap


# Input update and custom validations

@docs andUpdate

-}

import FormToolkit.Error as Error exposing (Error(..))
import FormToolkit.Field as Field exposing (Field(..), Msg)
import FormToolkit.Value as Value
import Internal.Field exposing (ParserResult(..), combineErrors)
import Internal.Utils as Utils
import Internal.Value
import Json.Decode
import Json.Encode
import RoseTree.Tree as Tree
import Time


{-| A parser that takes a tree of input data and returns a parsed result or an
error if the decoding fails.
-}
type Parser id a
    = Parser (Internal.Field.Field id (Error id) -> ParserResult id a)


{-| Parse for a field with the given identifier using a provided parser.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    type Fields
        = FirstName
        | LastName

    fields : Field Fields
    fields =
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

    fields
        |> parse (field FirstName string)
    --> Ok "Brian"

-}
field : id -> Parser id a -> Parser id a
field id parser =
    Parser
        (\tree ->
            case fieldHelp id (map2 (\_ a -> a) (Parser Internal.Field.validateTreeParser) parser) tree of
                ( Just (Success node a), path ) ->
                    Success (Tree.replaceAt path node tree) a

                ( Just (Failure node errors), path ) ->
                    Failure (Tree.replaceAt path node tree) errors

                ( Nothing, _ ) ->
                    Internal.Field.parseFailure tree (InputNotFound id)
        )


fieldHelp :
    id
    -> Parser id a
    -> Internal.Field.Field id (Error id)
    -> ( Maybe (ParserResult id a), List Int )
fieldHelp id (Parser parser) =
    Tree.foldWithPath
        (\path tree acc ->
            if (Tree.value tree |> .identifier) == Just id then
                ( Just (parser tree), path )

            else
                acc
        )
        ( Nothing, [] )


{-| Parses the input value as a `String`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "A string") ]
        |> parse string
        --> Ok "A string"

-}
string : Parser id String
string =
    Parser Internal.Field.parseString


{-| Parses the input value as an `Int`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.int 10) ]
        |> parse int
        --> Ok 10

-}
int : Parser id Int
int =
    parseValue
        (\id val ->
            Value.toInt val
                |> Result.fromMaybe (Error.NotNumber id)
        )


{-| Parses the input value as a `Float`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.float 10.5) ]
        |> parse float
        --> Ok 10.5

-}
float : Parser id Float
float =
    parseValue
        (\id val ->
            Value.toFloat val
                |> Result.fromMaybe (Error.NotNumber id)
        )


{-| Parses the input value as a `Bool`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.bool True) ]
        |> parse bool
        --> Ok True

-}
bool : Parser id Bool
bool =
    parseValue
        (\id val ->
            Value.toBool val
                |> Result.fromMaybe (Error.NotBool id)
        )


{-| Parses the input value as a
[Time.Posix](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix).
-}
posix : Parser id Time.Posix
posix =
    parseValue
        (\id val ->
            Value.toPosix val
                |> Result.fromMaybe (Error.ParseError id)
        )


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
    Parser
        (\node ->
            if Internal.Field.isBlank node then
                Success node Nothing

            else
                Internal.Field.parseMap Just parser node
        )


{-| Parses a list of inputs using the given parser.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.repeatable [ ]
        (Field.text [ ])
        [ Field.updateAttribute (Field.stringValue "mango")
        , Field.updateAttribute (Field.stringValue "banana")
        ]
        |> parse (list string)
        --> Ok [ "mango", "banana" ]

-}
list : Parser id a -> Parser id (List a)
list (Parser parser) =
    let
        listHelp =
            Tree.children
                >> List.foldr
                    (\node ( nodes, result ) ->
                        case parser node of
                            Success node2 a ->
                                ( node2 :: nodes
                                , Result.map2 (::) (Ok a) result
                                )

                            Failure node2 error2 ->
                                ( node2 :: nodes
                                , case result of
                                    Ok _ ->
                                        Err error2

                                    Err error ->
                                        let
                                            { identifier } =
                                                Tree.value node
                                        in
                                        Err (combineErrors identifier error error2)
                                )
                    )
                    ( [], Ok [] )
    in
    Parser
        (\node ->
            let
                ({ hidden } as attrs) =
                    Tree.value node
            in
            if hidden then
                Success (Tree.map (Tree.updateValue (\f -> { f | errors = [] })) node) []

            else
                let
                    ( children, result ) =
                        listHelp node

                    input2 =
                        Tree.branch attrs children
                in
                case result of
                    Ok elements ->
                        Success input2 elements

                    Err errors ->
                        Failure input2 errors
        )


{-| Tries a list of parsers in order until one succeeds.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "success!") ]
        |> parse (oneOf [ fail "not really ;)", string ])
        --> Ok "success!"


    Field.text [ Field.value (Value.string "success!") ]
        |> parse (oneOf [ string, fail "not really ;)" ])
        --> Ok "success!"

-}
oneOf : List (Parser id a) -> Parser id a
oneOf parsers =
    Parser (\node -> oneOfHelp parsers node Nothing)


oneOfHelp :
    List (Parser id a)
    -> Internal.Field.Field id (Error id)
    -> Maybe (Error id)
    -> ParserResult id a
oneOfHelp parsers input accError =
    let
        { identifier } =
            Tree.value input
    in
    case parsers of
        [] ->
            case accError of
                Nothing ->
                    Internal.Field.parseFailure input (ParseError identifier)

                Just error ->
                    Internal.Field.parseFailure input error

        (Parser parser) :: rest ->
            case parser input of
                Success input2 a ->
                    Success input2 a

                Failure _ newError ->
                    let
                        combinedError =
                            case accError of
                                Nothing ->
                                    newError

                                Just err ->
                                    combineErrors identifier err newError
                    in
                    oneOfHelp rest input (Just combinedError)


{-| Returns the raw value of the input without any decoding.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "A string") ]
        |> parse value
        --> Ok (Value.string "A string")

-}
value : Parser id Value.Value
value =
    parseValue (always Ok)


{-| -}
email : Parser id String
email =
    Parser Internal.Field.parseEmail


parseValue : (Maybe id -> Value.Value -> Result (Error id) a) -> Parser id a
parseValue =
    Parser << Internal.Field.parseValue


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
            [ Field.updateAttribute (Field.stringValue "mango")
            , Field.updateAttribute (Field.stringValue "banana")
            ]
        ]
        |> parse json
        |> Result.map (Json.Encode.encode 0)
        --> Ok "{\"first-name\":\"Brian\",\"last-name\":\"Eno\",\"fruits\":[{\"fruit\":\"mango\"},{\"fruit\":\"banana\"}]}"

-}
json : Parser id Json.Decode.Value
json =
    map2 (always identity)
        (Parser Internal.Field.validateTreeParser)
        (Parser
            (\input ->
                case jsonEncodeObject input of
                    Ok a ->
                        Success input a

                    Err err ->
                        Internal.Field.parseFailure input err
            )
        )


jsonEncodeObject : Internal.Field.Field id (Error id) -> Result (Error id) Json.Encode.Value
jsonEncodeObject input =
    jsonEncodeHelp input [] |> Result.map Json.Encode.object


jsonEncodeHelp :
    Internal.Field.Field id (Error id)
    -> List ( String, Json.Decode.Value )
    -> Result (Error id) (List ( String, Json.Decode.Value ))
jsonEncodeHelp input acc =
    let
        ({ name, inputType, identifier } as attrs) =
            Tree.value input

        accumulate jsonValue =
            case name of
                Just n ->
                    Ok (( n, jsonValue ) :: acc)

                Nothing ->
                    Err
                        (HasNoName identifier)
    in
    case inputType of
        Internal.Field.Group ->
            case name of
                Nothing ->
                    Tree.children input
                        |> List.foldr (\e -> Result.andThen (jsonEncodeHelp e))
                            (Ok acc)

                _ ->
                    Tree.children input
                        |> List.foldr (\e -> Result.andThen (jsonEncodeHelp e))
                            (Ok [])
                        |> Result.map Json.Encode.object
                        |> Result.andThen accumulate

        Internal.Field.Repeatable _ ->
            Tree.children input
                |> List.foldr (\e -> Result.map2 (::) (jsonEncodeObject e)) (Ok [])
                |> Result.map (Json.Encode.list identity)
                |> Result.andThen accumulate

        _ ->
            case name of
                Just n ->
                    Ok
                        (( n
                         , Internal.Value.encode attrs.value
                         )
                            :: acc
                        )

                Nothing ->
                    Ok acc


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
    Parser (\node -> Internal.Field.parseSuccess node a)


{-| A parser that always fails with a custom error.
-}
fail : String -> Parser id a
fail err =
    Parser
        (\node ->
            Internal.Field.parseFailure node
                (Error.CustomError (Tree.value node |> .identifier) err)
        )


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
    Parser (Internal.Field.parseAndUpdate updateFunc parser)


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
    Parser
        (Internal.Field.parseAndThen
            (\a ->
                case func a of
                    Parser p ->
                        p
            )
            parser
        )


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

    fields : Field Fields
    fields =
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


    fields
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
    Parser (Internal.Field.parseMap func parser)


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
    Parser
        (\node ->
            case a node of
                Success tree2 res ->
                    case b tree2 of
                        Success tree3 res2 ->
                            Success tree3 (func res res2)

                        Failure tree3 error ->
                            Failure tree3 error

                Failure tree2 error ->
                    case b tree2 of
                        Success tree3 _ ->
                            Failure tree3 error

                        Failure tree3 error2 ->
                            let
                                { identifier } =
                                    Tree.value node
                            in
                            Failure tree3
                                (combineErrors identifier error2 error)
        )


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

    fields : Field String
    fields =
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

    fields
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


{-| Parses an input.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    Field.text
        [ Field.value (Value.string "A string")
        , Field.required True
        ]
        |> parse string
    --> Ok "A string"

-}
parse : Parser id a -> Field id -> Result (Error id) a
parse parser input =
    parseToTuple parser input |> Tuple.second


parseToTuple : Parser id a -> Field id -> ( Field id, Result (Error id) a )
parseToTuple parser (Field input) =
    let
        (Parser fn) =
            map2 (\a _ -> a) parser (Parser Internal.Field.validateNodeParser)
    in
    case fn input of
        Success input2 a ->
            ( Field input2, Ok a )

        Failure input2 error ->
            ( Field input2, Err error )


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
parseUpdate : Parser id a -> Msg id -> Field id -> ( Field id, Result (Error id) a )
parseUpdate parser (Field.Msg msg) (Field input) =
    Internal.Field.update msg input |> Field |> parseToTuple parser


{-| Parses an input and updates the tree revealing errors.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    Field.text
        [ Field.value (Value.string "A string")
        , Field.required True
        ]
        |> parseValidate string
        |> Tuple.second
    -->  Ok "A string"

-}
parseValidate : Parser id a -> Field id -> ( Field id, Result (Error id) a )
parseValidate parser input =
    parseToTuple parser input
        |> Tuple.mapFirst (\(Field f) -> Internal.Field.touchTree f)
        |> Tuple.mapFirst Field


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
        |> andThen
            (Parser
                << Internal.Field.parseMaskedString (Utils.parseMask mask)
            )
