module FormToolkit.Parse exposing
    ( Parser
    , field
    , string, int, float, bool, posix, maybe, list
    , value, customValue, json
    , succeed, fail, custom, format
    , map, map2, map3, map4, map5, map6, map7, map8
    , andThen, andMap
    , parse, validateAndParse
    , errorToFieldIdentifier
    )

{-| Map the values of an input or group of inputs to any shape you want, if you
know `Json.Decode` you know how to use this module ;)

@docs Parser


# Decoding functions

@docs field
@docs string, int, float, bool, posix, maybe, list
@docs value, customValue, json
@docs succeed, fail, custom, format


# Maps, combinators and pipeline style decoding

@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap


# Decoding

@docs parse, validateAndParse
@docs errorToFieldIdentifier

-}

import Dict
import FormToolkit.Error exposing (Error(..))
import FormToolkit.Field exposing (Field(..))
import FormToolkit.Value as Value
import Internal.Field
import Internal.Value
import Json.Decode
import Json.Encode
import List.Extra
import RoseTree.Tree as Tree
import Time


type alias InternalField id val =
    Internal.Field.Field id val (Error id val)


type Partial id val a
    = Failure (InternalField id val) (List (Error id val))
    | Success (InternalField id val) a


{-| A parser that takes a tree of input data and returns a parsed result or an
error if the decoding fails.
-}
type Parser id val a
    = Parser (InternalField id val -> Partial id val a)


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
field id parser =
    Parser
        (\tree ->
            case fieldHelp id parser tree of
                ( Just (Success node a), path ) ->
                    Success (Tree.replaceAt path node tree) a

                ( Just (Failure node errors), path ) ->
                    Failure (Tree.replaceAt path node tree) errors

                ( Nothing, _ ) ->
                    failure (Field tree) (InputNotFound id)
        )


fieldHelp : id -> Parser id val a -> InternalField id val -> ( Maybe (Partial id val a), List Int )
fieldHelp id parser =
    Tree.foldWithPath
        (\path tree acc ->
            if Internal.Field.identifier tree == Just id then
                ( Just (apply parser tree), path )

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
maybe parser =
    Parser
        (\input ->
            if Internal.Field.isBlank input then
                Success input Nothing

            else
                mapHelp Just parser input
        )


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
list parser =
    Parser
        (\input ->
            let
                ( children, result ) =
                    listHelp parser input

                input2 =
                    Tree.branch (Tree.value input) children
            in
            case result of
                Ok elements ->
                    Success input2 elements

                Err errors ->
                    Failure input2 errors
        )


listHelp :
    Parser id val a
    -> InternalField id val
    -> ( List (InternalField id val), Result (List (Error id val)) (List a) )
listHelp parser =
    Tree.children
        >> List.foldr
            (\node ( nodes, result ) ->
                case apply parser node of
                    Success node2 a ->
                        ( node2 :: nodes
                        , Result.map2 (::) (Ok a) result
                        )

                    Failure node2 errors2 ->
                        ( node2 :: nodes
                        , case result of
                            Ok _ ->
                                Err errors2

                            Err errors ->
                                Err (List.Extra.unique (errors2 ++ errors))
                        )
            )
            ( [], Ok [] )


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

Tipically used for `select` or `radio` inputs with options of custom value, but
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

Usefull if you just one to forward the form values to a backend.

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
    Parser
        (\input ->
            case jsonEncodeObject input of
                Ok a ->
                    Success input a

                Err err ->
                    failure (Field input) err
        )


jsonEncodeObject : InternalField id val -> Result (Error id val) Json.Encode.Value
jsonEncodeObject input =
    jsonEncodeHelp input [] |> Result.map Json.Encode.object


jsonEncodeHelp :
    InternalField id val
    -> List ( String, Json.Decode.Value )
    -> Result (Error id val) (List ( String, Json.Decode.Value ))
jsonEncodeHelp input acc =
    let
        accumulate jsonValue =
            case Internal.Field.name input of
                Just name ->
                    Ok (( name, jsonValue ) :: acc)

                Nothing ->
                    Err
                        (RepeatableHasNoName
                            (Internal.Field.identifier input)
                        )
    in
    case Internal.Field.inputType input of
        Internal.Field.Group ->
            case Internal.Field.name input of
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
            case Internal.Field.name input of
                Just name ->
                    Ok
                        (( name
                         , Internal.Value.encode (Internal.Field.value input)
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
custom func =
    parseHelp
        (\input ->
            func (Value.Value (Internal.Field.value input))
                |> Result.mapError
                    (CustomError (Internal.Field.identifier input))
        )


{-| -}
format : (String -> String) -> Parser id val String
format func =
    Parser
        (\input ->
            case
                Internal.Field.value input
                    |> Internal.Value.toString
            of
                Just str ->
                    Success
                        (Tree.updateValue
                            (\attrs ->
                                { attrs | value = Internal.Value.Text (func str) }
                            )
                            input
                        )
                        ()

                Nothing ->
                    Success input ()
        )
        |> andThen (\() -> string)


parseValue : (Value.Value val -> Maybe a) -> Parser id val a
parseValue func =
    parseHelp
        (\input ->
            if Internal.Field.isGroup input then
                Err (IsGroupNotInput (Internal.Field.identifier input))

            else
                Internal.Value.toString (Internal.Field.value input)
                    |> Maybe.andThen
                        (\key ->
                            Internal.Field.options input
                                |> Dict.fromList
                                |> Dict.get key
                        )
                    |> Maybe.withDefault (Internal.Field.value input)
                    |> (func << Value.Value)
                    |> Maybe.map Ok
                    |> Maybe.withDefault
                        (Err (ParseError (Internal.Field.identifier input)))
        )


parseHelp : (InternalField id val -> Result (Error id val) a) -> Parser id val a
parseHelp func =
    Parser
        (\input ->
            case func input of
                Ok a ->
                    Success input a

                Err err ->
                    failure (Field input) err
        )


failure : Field id val -> Error id val -> Partial id val a
failure (Field input) err =
    Failure (Internal.Field.setErrors [ err ] input) [ err ]


{-| Chains together parsers that depend on previous decoding results.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    wordsParser : Parser id val (List String)
    wordsParser =
        int
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
    Parser
        (\input ->
            case parser input of
                Success input2 a ->
                    apply (func a) input2

                Failure input2 errors ->
                    Failure input2 errors
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
map func parser =
    Parser (mapHelp func parser)


mapHelp : (a -> b) -> Parser id val a -> InternalField id val -> Partial id val b
mapHelp func (Parser parser) input =
    case parser input of
        Success input2 a ->
            Success input2 (func a)

        Failure input2 errors ->
            Failure input2 errors


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
map2 func a b =
    Parser
        (\input ->
            case apply a input of
                Success input2 res ->
                    case apply b input2 of
                        Success input3 res2 ->
                            Success input3 (func res res2)

                        Failure input3 errors ->
                            Failure input3 errors

                Failure tree2 errors ->
                    case apply b tree2 of
                        Success input3 _ ->
                            Failure input3 errors

                        Failure input3 errors2 ->
                            Failure input3 (List.Extra.unique (errors2 ++ errors))
        )


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



-- Field.text [ Field.value (Value.string "A string"), Field.required True ]
--     |> parse string
--     --> Ok "A string"


{-| Parses an input using the given parser without applying field validations
(required, min, max...).
-}
parse : Parser id val a -> Field id val -> Result (List (Error id val)) a
parse parser (Field input) =
    case apply parser input of
        Success _ a ->
            Ok a

        Failure _ errors ->
            Err errors


{-| Validates and parses an input using the given parser.
-}
validateAndParse :
    Parser id val a
    -> Field id val
    -> ( Field id val, Result (List (Error id val)) a )
validateAndParse parser (Field input) =
    case apply (validateTree |> andThen (\() -> parser)) input of
        Success input2 a ->
            ( Field input2, Ok a )

        Failure input2 errors ->
            ( Field input2, Err errors )


validateTree : Parser id val ()
validateTree =
    Parser
        (\input ->
            let
                validators =
                    [ checkRequired, checkInRange, checkOptionsProvided ]

                updated =
                    Tree.map
                        (\node ->
                            case List.filterMap ((|>) node) validators of
                                [] ->
                                    node

                                errors ->
                                    Internal.Field.setErrors errors node
                        )
                        input
            in
            case Internal.Field.errors updated of
                [] ->
                    Success updated ()

                errors ->
                    Failure updated errors
        )


apply : Parser id val a -> InternalField id val -> Partial id val a
apply (Parser parser) =
    parser


checkRequired : InternalField id val -> Maybe (Error id val)
checkRequired input =
    if
        Internal.Field.isRequired input
            && Internal.Field.isBlank input
    then
        Just (IsBlank (Internal.Field.identifier input))

    else
        Nothing


checkInRange : InternalField id val -> Maybe (Error id val)
checkInRange tree =
    let
        val =
            Value.Value (Internal.Field.value tree)

        min =
            Value.Value (Internal.Field.min tree)

        max =
            Value.Value (Internal.Field.max tree)
    in
    case
        ( Internal.Value.compare
            (Internal.Field.value tree)
            (Internal.Field.min tree)
        , Internal.Value.compare
            (Internal.Field.value tree)
            (Internal.Field.max tree)
        )
    of
        ( Just LT, Just _ ) ->
            Just
                (ValueNotInRange (Internal.Field.identifier tree)
                    { value = val, min = min, max = max }
                )

        ( Just _, Just GT ) ->
            Just
                (ValueNotInRange (Internal.Field.identifier tree)
                    { value = val, min = min, max = max }
                )

        ( Just LT, Nothing ) ->
            Just
                (ValueTooSmall (Internal.Field.identifier tree)
                    { value = val, min = min }
                )

        ( Nothing, Just GT ) ->
            Just
                (ValueTooLarge (Internal.Field.identifier tree)
                    { value = val, max = max }
                )

        _ ->
            Nothing


checkOptionsProvided : InternalField id val -> Maybe (Error id val)
checkOptionsProvided input =
    case
        ( Internal.Field.inputType input
        , Internal.Field.options input
        )
    of
        ( Internal.Field.Select, [] ) ->
            Just (NoOptionsProvided (Internal.Field.identifier input))

        ( Internal.Field.Radio, [] ) ->
            Just (NoOptionsProvided (Internal.Field.identifier input))

        ( Internal.Field.StrictAutocomplete, [] ) ->
            Just (NoOptionsProvided (Internal.Field.identifier input))

        _ ->
            Nothing


{-| Obtain the indentifier for the field corresponding to the error, if the
field has identifier.
-}
errorToFieldIdentifier : Error id val -> Maybe id
errorToFieldIdentifier error =
    case error of
        ValueTooLarge maybeId _ ->
            maybeId

        ValueTooSmall maybeId _ ->
            maybeId

        ValueNotInRange maybeId _ ->
            maybeId

        IsGroupNotInput maybeId ->
            maybeId

        IsBlank maybeId ->
            maybeId

        CustomError maybeId _ ->
            maybeId

        ListError maybeId _ ->
            maybeId

        InputNotFound id ->
            Just id

        RepeatableHasNoName maybeId ->
            maybeId

        NoOptionsProvided maybeId ->
            maybeId

        ParseError maybeId ->
            maybeId
