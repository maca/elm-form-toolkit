module FormToolkit.Parse exposing
    ( Parser
    , field
    , string, int, float, bool, posix, maybe, list
    , value, customValue, json
    , succeed, fail, custom, format
    , map, map2, map3, map4, map5, map6, map7, map8
    , andThen, andMap
    , parse, validateAndParse
    , Error(..), errorToFieldIdentifier
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
@docs Error, errorToFieldIdentifier

-}

import Dict
import FormToolkit.Value as Value
import Internal.Field as Field
import Internal.Value
import Json.Decode
import Json.Encode
import List.Extra
import RoseTree.Tree as Tree
import Time


type Partial id val a
    = Failure (Field id val) (List (Error id val))
    | Success (Field id val) a


{-| A decoder that takes a tree of input data and returns a decoded result or an
error if the decoding fails.
-}
type Parser id val a
    = Parser (Field id val -> Partial id val a)


type alias Field id val =
    Field.Field id val (Error id val)


{-| Parse for a field with the given identifier using a provided decoder.

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

    form |> decode (field FirstName string)
    --> Ok "Brian"

-}
field : id -> Parser id val a -> Parser id val a
field id decoder =
    Parser
        (\tree ->
            case fieldHelp id decoder tree of
                ( Just (Success node a), path ) ->
                    Success (Tree.replaceAt path node tree) a

                ( Just (Failure node errors), path ) ->
                    Failure (Tree.replaceAt path node tree) errors

                ( Nothing, _ ) ->
                    failure tree (InputNotFound id)
        )


fieldHelp : id -> Parser id val a -> Field id val -> ( Maybe (Partial id val a), List Int )
fieldHelp id decoder =
    Tree.foldWithPath
        (\path tree acc ->
            if Field.identifier tree == Just id then
                ( Just (apply decoder tree), path )

            else
                acc
        )
        ( Nothing, [] )


{-| Decodes the input value as a `String`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "A string") ]
        |> decode string
        --> Ok "A string"

-}
string : Parser id val String
string =
    parseValue Value.toString


{-| Decodes the input value as an `Int`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.int 10) ]
        |> decode int
        --> Ok 10

-}
int : Parser id val Int
int =
    parseValue Value.toInt


{-| Decodes the input value as a `Float`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.float 10.5) ]
        |> decode float
        --> Ok 10.5

-}
float : Parser id val Float
float =
    parseValue Value.toFloat


{-| Decodes the input value as a `Bool`.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.bool True) ]
        |> decode bool
        --> Ok True

-}
bool : Parser id val Bool
bool =
    parseValue Value.toBool


{-| Decodes the input value as a
[Time.Posix](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix).
-}
posix : Parser id val Time.Posix
posix =
    parseValue Value.toPosix


{-| Allows dealing with blank values without producing an error.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "A string") ]
        |> decode (maybe string)
        --> Ok (Just "A string")

    Field.text []
        |> decode (maybe string)
        --> Ok Nothing

-}
maybe : Parser id val a -> Parser id val (Maybe a)
maybe decoder =
    Parser
        (\input ->
            if Field.isBlank input then
                Success input Nothing

            else
                mapHelp Just decoder input
        )


{-| Decodes a list of inputs using the given decoder.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.repeatable [ ]
        (Field.text [ ])
        [ Field.updateAttribute
            (Field.value (Value.string "mango") )
        , Field.updateAttribute
            (Field.value (Value.string "banana") )
        ]
        |> decode (list string)
        --> Ok [ "mango", "banana" ]

-}
list : Parser id val a -> Parser id val (List a)
list decoder =
    Parser
        (\input ->
            let
                ( children, result ) =
                    listHelp decoder input

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
    -> Field id val
    -> ( List (Field id val), Result (List (Error id val)) (List a) )
listHelp decoder =
    Tree.children
        >> List.foldr
            (\node ( nodes, result ) ->
                case apply decoder node of
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
        |> decode value
        --> Ok (Value.string "A string")

-}
value : Parser id val (Value.Value val)
value =
    parseValue Just


{-| Decodes the custom value of an input.

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

    langSelect |> decode customValue
    --> Ok ES

    autocomplete |> decode customValue
    --> Ok EN

-}
customValue : Parser id val val
customValue =
    parseValue Value.toCustom


{-| Converts the entire input tree into a JSON
[Value](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode#Value).
Field `name` property will be used as the key, if an input name is not present
the decoder will fail.

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
        |> decode json
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
                    failure input err
        )


jsonEncodeObject : Field id val -> Result (Error id val) Json.Encode.Value
jsonEncodeObject input =
    jsonEncodeHelp input [] |> Result.map Json.Encode.object


jsonEncodeHelp :
    Field id val
    -> List ( String, Json.Decode.Value )
    -> Result (Error id val) (List ( String, Json.Decode.Value ))
jsonEncodeHelp input acc =
    let
        accumulate jsonValue =
            case Field.name input of
                Just name ->
                    Ok (( name, jsonValue ) :: acc)

                Nothing ->
                    Err (RepeatableHasNoName (Field.identifier input))
    in
    case Field.inputType input of
        Field.Group ->
            case Field.name input of
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

        Field.Repeatable _ ->
            Tree.children input
                |> List.foldr (\e -> Result.map2 (::) (jsonEncodeObject e)) (Ok [])
                |> Result.map (Json.Encode.list identity)
                |> Result.andThen accumulate

        _ ->
            case Field.name input of
                Just name ->
                    Ok (( name, Internal.Value.encode (Field.value input) ) :: acc)

                Nothing ->
                    Ok acc


{-| A decoder that always succeeds with the given value, use for buiding
decoding pipelines with [andMap](#andMap), or to chain decoders with
[andThen](#andThen).

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    type Special
        = SpecialValue

    specialParse : Parse id val Special
    specialParse =
        string
            |> andThen
                (\strValue ->
                    if strValue == "special" then
                        succeed SpecialValue

                    else
                        fail "Nothing special"
                )


    Field.text [ Field.value (Value.string "special") ] |> decode specialParse
    --> Ok SpecialValue

-}
succeed : a -> Parser id val a
succeed a =
    custom (always (Ok a))


{-| A decoder that always fails with a custom error.
-}
fail : String -> Parser id val a
fail err =
    custom (always (Err err))


{-| -}
custom : (Value.Value val -> Result String a) -> Parser id val a
custom func =
    parseHelp
        (\input ->
            func (Value.Value (Field.value input))
                |> Result.mapError
                    (CustomError (Field.identifier input))
        )


{-| -}
format : (String -> String) -> Parser id val String
format func =
    Parser
        (\input ->
            case Field.value input |> Internal.Value.toString of
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
            if Field.isGroup input then
                Err (IsGroupNotInput (Field.identifier input))

            else
                Internal.Value.toString (Field.value input)
                    |> Maybe.andThen
                        (\key ->
                            Field.options input
                                |> Dict.fromList
                                |> Dict.get key
                        )
                    |> Maybe.withDefault (Field.value input)
                    |> (func << Value.Value)
                    |> Maybe.map Ok
                    |> Maybe.withDefault
                        (Err (ParseError (Field.identifier input)))
        )


parseHelp : (Field id val -> Result (Error id val) a) -> Parser id val a
parseHelp func =
    Parser
        (\input ->
            case func input of
                Ok a ->
                    Success input a

                Err err ->
                    failure input err
        )


failure : Field id val -> Error id val -> Partial id val a
failure input err =
    Failure (Field.setErrors [ err ] input) [ err ]


{-| Chains together decoders that depend on previous decoding results.

    -- justinmimbs/date
    import Date exposing (Date)

    dateParse : Parse id val Date
    dateParse =
        string
            |> andThen
                (\strValue ->
                    case Date.fromString "dd.MM.yyyy" strValue of
                        Ok date ->
                            succeed date

                        Err err ->
                            fail err
                )


    Field.text [ Field.value (Value.string "07.03.1981") ]
        |> decode dateParse
        --> Ok (Date.fromCalendarDate 1981 Date.March 7)

-}
andThen : (a -> Parser id val b) -> Parser id val a -> Parser id val b
andThen func (Parser decoder) =
    Parser
        (\input ->
            case decoder input of
                Success input2 a ->
                    apply (func a) input2

                Failure input2 errors ->
                    Failure input2 errors
        )


{-| Incrementally apply decoders in a pipeline fashion.

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

    personParse : Parse Fields val Person
    personParse =
        succeed Person
            |> andMap (field FirstName string)
            |> andMap (field LastName string)
            |> andMap (field Age int)


    form |> decode personParse
    --> Ok { firstName = "Penny", lastName = "Rimbaud", age = 81 }

-}
andMap : Parser id val a -> Parser id val (a -> b) -> Parser id val b
andMap a b =
    map2 (|>) a b


{-| Transforms the result of a decoder using a function.

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "a string") ]
        |> decode (map String.toUpper string)
        --> Ok "A STRING"

-}
map : (a -> b) -> Parser id val a -> Parser id val b
map func decoder =
    Parser (mapHelp func decoder)


mapHelp : (a -> b) -> Parser id val a -> Field id val -> Partial id val b
mapHelp func (Parser decoder) input =
    case decoder input of
        Success input2 a ->
            Success input2 (func a)

        Failure input2 errors ->
            Failure input2 errors


{-| Combines two decoders using a function.

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
        |> decode
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


{-| Combines three decoders using a function.

    import FormToolkit.Field as Field exposing (Field)
    import FormToolkit.Value as Value

    type alias Person =
        { firstName : String
        , lastName : String
        , age : Int
        }

    personParse : Parse String val Person
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

    form |> decode personParse
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


{-| Decodes an input using the given decoder without applying field validations
(required, min, max...).

    import FormToolkit.Field as Field
    import FormToolkit.Value as Value

    Field.text [ Field.value (Value.string "A string"), Field.required True ]
        |> decode string
        --> Ok "A string"

    Field.text
        [ Field.value (Value.bool True)
        , Field.identifier "MyField"
        ]
        |> decode string
        --> Err [ ParseError (Just "MyField") ]

-}
parse : Parser id val a -> Field id val -> Result (List (Error id val)) a
parse decoder input =
    case apply decoder input of
        Success _ a ->
            Ok a

        Failure _ errors ->
            Err errors


{-| Validates and decodes an input using the given decoder.
-}
validateAndParse :
    Parser id val a
    -> Field id val
    -> ( Field id val, Result (List (Error id val)) a )
validateAndParse decoder input =
    case apply (validateTree |> andThen (\() -> decoder)) input of
        Success input2 a ->
            ( input2, Ok a )

        Failure input2 errors ->
            ( input2, Err errors )


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
                                    Field.setErrors errors node
                        )
                        input
            in
            case Field.errors updated of
                [] ->
                    Success updated ()

                errors ->
                    Failure updated errors
        )


apply : Parser id val a -> Field id val -> Partial id val a
apply (Parser decoder) =
    decoder


checkRequired : Field id val -> Maybe (Error id val)
checkRequired input =
    if Field.isRequired input && Field.isBlank input then
        Just (IsBlank (Field.identifier input))

    else
        Nothing


checkInRange : Field id val -> Maybe (Error id val)
checkInRange tree =
    let
        val =
            Value.Value (Field.value tree)

        min =
            Value.Value (Field.min tree)

        max =
            Value.Value (Field.max tree)
    in
    case
        ( Internal.Value.compare (Field.value tree) (Field.min tree)
        , Internal.Value.compare (Field.value tree) (Field.max tree)
        )
    of
        ( Just LT, Just _ ) ->
            Just
                (ValueNotInRange (Field.identifier tree)
                    { value = val, min = min, max = max }
                )

        ( Just _, Just GT ) ->
            Just
                (ValueNotInRange (Field.identifier tree)
                    { value = val, min = min, max = max }
                )

        ( Just LT, Nothing ) ->
            Just
                (ValueTooSmall (Field.identifier tree)
                    { value = val, min = min }
                )

        ( Nothing, Just GT ) ->
            Just
                (ValueTooLarge (Field.identifier tree)
                    { value = val, max = max }
                )

        _ ->
            Nothing


checkOptionsProvided : Field id val -> Maybe (Error id val)
checkOptionsProvided input =
    case ( Field.inputType input, Field.options input ) of
        ( Field.Select, [] ) ->
            Just (NoOptionsProvided (Field.identifier input))

        ( Field.Radio, [] ) ->
            Just (NoOptionsProvided (Field.identifier input))

        ( Field.StrictAutocomplete, [] ) ->
            Just (NoOptionsProvided (Field.identifier input))

        _ ->
            Nothing


{-| Represents an error that occurred during decoding or validation.
-}
type Error id val
    = ValueTooLarge (Maybe id) { value : Value.Value val, max : Value.Value val }
    | ValueTooSmall (Maybe id) { value : Value.Value val, min : Value.Value val }
    | ValueNotInRange
        (Maybe id)
        { value : Value.Value val
        , min : Value.Value val
        , max : Value.Value val
        }
    | IsBlank (Maybe id)
    | CustomError (Maybe id) String
    | ListError (Maybe id) { index : Int, error : Error id val }
    | InputNotFound id
    | RepeatableHasNoName (Maybe id)
    | IsGroupNotInput (Maybe id)
    | NoOptionsProvided (Maybe id)
    | ParseError (Maybe id)


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
