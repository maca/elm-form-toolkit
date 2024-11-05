module FormToolkit.Decode exposing
    ( Decoder
    , field
    , string, int, float, bool, posix, maybe, list
    , value, customValue, json
    , succeed, fail, custom, format
    , map, map2, map3, map4, map5, map6, map7, map8
    , andThen, andMap
    , decode, validateAndDecode
    , Error(..), errorToFieldIdentifier
    )

{-| Map the values of an input or group of inputs to any shape you want, if you
know `Json.Decode` you know how to use this module ;)

@docs Decoder


# Decoding functions

@docs field
@docs string, int, float, bool, posix, maybe, list
@docs value, customValue, json
@docs succeed, fail, custom, format


# Maps, combinators and pipeline style decoding

@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap


# Decoding

@docs decode, validateAndDecode
@docs Error, errorToFieldIdentifier

-}

import Dict
import FormToolkit.Value as Value
import Internal.Input as Input
import Internal.Value
import Json.Decode
import Json.Encode
import List.Extra
import RoseTree.Tree as Tree
import Time


type Partial id val a
    = Failure (Input id val) (List (Error id val))
    | Success (Input id val) a


{-| A decoder that takes a tree of input data and returns a decoded result or an
error if the decoding fails.
-}
type Decoder id val a
    = Decoder (Input id val -> Partial id val a)


type alias Input id val =
    Input.Input id val (Error id val)


{-| Decoder for a field with the given identifier using a provided decoder.


    type Fields
        = FirstName
        | LastName

    form : Input Fields val
    form =
        Input.group []
            [ Input.text
                [ Input.label "First name"
                , Input.identifier FirstName
                , Input.value (Value.string "Brian")
                ]
            , Input.text
                [ Input.label "Last name"
                , Input.identifier LastName
                , Input.value (Value.string "Eno")
                ]
            ]

    decoded =
        form |> decode (field FirstName string)

    -- Ok "Brian"

-}
field : id -> Decoder id val a -> Decoder id val a
field id decoder =
    Decoder
        (\tree ->
            case fieldHelp id decoder tree of
                ( Just (Success node a), path ) ->
                    Success (Tree.replaceAt path node tree) a

                ( Just (Failure node errors), path ) ->
                    Failure (Tree.replaceAt path node tree) errors

                ( Nothing, _ ) ->
                    failure tree (InputNotFound id)
        )


fieldHelp : id -> Decoder id val a -> Input id val -> ( Maybe (Partial id val a), List Int )
fieldHelp id decoder =
    Tree.foldWithPath
        (\path tree acc ->
            if Input.identifier tree == Just id then
                ( Just (apply decoder tree), path )

            else
                acc
        )
        ( Nothing, [] )


{-| Decodes the input value as a `String`.

    Input.text [ Input.value (Value.string "A string") ]
        |> decode string
        == Ok "A string"

-}
string : Decoder id val String
string =
    parseValue Value.toString


{-| Decodes the input value as an `Int`.

    Input.text [ Input.value (Value.int 10) ]
        |> decode int
        == Ok 10

-}
int : Decoder id val Int
int =
    parseValue Value.toInt


{-| Decodes the input value as a `Float`.

    Input.text [ Input.value (Value.float 10.5) ]
        |> decode float
        == Ok 10.5

-}
float : Decoder id val Float
float =
    parseValue Value.toFloat


{-| Decodes the input value as a `Bool`.

    Input.text [ Input.value (Value.bool True) ]
        |> decode bool
        == Ok True

-}
bool : Decoder id val Bool
bool =
    parseValue Value.toBool


{-| Decodes the input value as a
[Time.Posix](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix).

    Input.date [ Input.value (Value.time (Time.millisToPosix 0)) ]
        |> (posix |> map (Time.toYear Time.utc))
        == Ok 1970

-}
posix : Decoder id val Time.Posix
posix =
    parseValue Value.toPosix


{-| Allows dealing with blank values without producing an error.

    Input.text [ Input.value (Value.string "A string") ]
        |> decode (maybe string)
        == Ok (Just "A string")

    Input.text []
        |> decode (maybe string)
        == Ok Nothing

-}
maybe : Decoder id val a -> Decoder id val (Maybe a)
maybe decoder =
    Decoder
        (\input ->
            if Input.isBlank input then
                Success input Nothing

            else
                mapHelp Just decoder input
        )


{-| Decodes a list of inputs using the given decoder.

    Input.repeatable []
        (Input.text [])
        [ Input.text [ Input.value (Value.string "mango") ]
        , Input.text [ Input.value (Value.string "banana") ]
        ]
        |> decode (list string)
        == Ok [ "mango", "banana" ]

-}
list : Decoder id val a -> Decoder id val (List a)
list decoder =
    Decoder
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
    Decoder id val a
    -> Input id val
    -> ( List (Input id val), Result (List (Error id val)) (List a) )
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

    Input.text [ Input.value (Value.string "A string") ]
        |> decode value
        == Value.string "A string"

-}
value : Decoder id val (Value.Value val)
value =
    parseValue Just


{-| Decodes the custom value of an input.

Tipically used for `select` or `radio` inputs with options of custom value, but
also for autocompleatable text inputs where the inputted text corresponds to an
option text.

    type Lang
        = ES
        | EN
        | DE

    langSelect : Input id Lang
    langSelect =
        Input.select
            [ Input.label "Language"
            , Input.value (Value.custom ES)
            , Input.options
                [ ( "Español", Value.custom ES )
                , ( "English", Value.custom EN )
                , ( "Deutsch", Value.custom DE )
                ]
            ]

    autocomplete : Input id Lang
    autocomplete =
        Input.text
            [ Input.value (Value.string "English")
            , Input.options
                [ ( "Español", Value.custom ES )
                , ( "English", Value.custom EN )
                , ( "Deutsch", Value.custom DE )
                ]
            ]

    es =
        langSelect |> decode customValue == Ok ES

    en =
        autocomplete |> decode customValue == Ok EN

-}
customValue : Decoder id val val
customValue =
    parseValue Value.toCustom


{-| Converts the entire input tree into a JSON
[Value](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode#Value).
Input [name](FormToolkit.Input#name) property will be used as the key,
if an input [name](FormToolkit.Input#name) is not present the decoder will fail.

Usefull if you just one to forward the form values to a backend.

    Input.group []
        [ Input.text
            [ Input.label "First name"
            , Input.name "first-name"
            , Input.value (Value.string "Brian")
            ]
        , Input.text
            [ Input.label "Last name"
            , Input.name "last-name"
            , Input.value (Value.string "Eno")
            ]
        , Input.repeatable [ Input.name "fruits" ]
            (Input.text [])
            [ Input.text
                [ Input.name "fruit"
                , Input.value (Value.string "mango")
                ]
            , Input.text
                [ Input.name "fruit"
                , Input.value (Value.string "banana")
                ]
            ]
        ]
        |> decode json
        |> Result.map (Json.Encode.encode 0)
        == Ok "{\"first-name\":\"Brian\",\"last-name\":\"Eno\",\"fruits\":[{\"fruit\":\"mango\"},{\"fruit\":\"banana\"}]}"

-}
json : Decoder id val Json.Decode.Value
json =
    Decoder
        (\input ->
            case jsonEncodeObject input of
                Ok a ->
                    Success input a

                Err err ->
                    failure input err
        )


jsonEncodeObject : Input id val -> Result (Error id val) Json.Encode.Value
jsonEncodeObject input =
    jsonEncodeHelp input [] |> Result.map Json.Encode.object


jsonEncodeHelp :
    Input id val
    -> List ( String, Json.Decode.Value )
    -> Result (Error id val) (List ( String, Json.Decode.Value ))
jsonEncodeHelp input acc =
    let
        accumulate jsonValue =
            case Input.name input of
                Just name ->
                    Ok (( name, jsonValue ) :: acc)

                Nothing ->
                    Err (RepeatableHasNoName (Input.identifier input))
    in
    case Input.inputType input of
        Input.Group ->
            case Input.name input of
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

        Input.Repeatable _ ->
            Tree.children input
                |> List.foldr (\e -> Result.map2 (::) (jsonEncodeObject e)) (Ok [])
                |> Result.map (Json.Encode.list identity)
                |> Result.andThen accumulate

        _ ->
            case Input.name input of
                Just name ->
                    Ok (( name, Internal.Value.encode (Input.value input) ) :: acc)

                Nothing ->
                    Ok acc


{-| A decoder that always succeeds with the given value, use for buiding
decoding pipelines with [andMap](#andMap), or to chain decoders with
[andThen](#andThen).


    type SpecialValue
        = SpecialValue

    specialDecoder : Decoder id val Special
    specialDecoder =
        string
            |> andThen
                (\strValue ->
                    if strValue == "special" then
                        succeed SpecialValue

                    else
                        fail "Nothing special"
                )

    result =
        Input.text [ Input.value (Value.string "special") ]
            |> decode specialDecoder

    --> Ok SpecialValue

-}
succeed : a -> Decoder id val a
succeed a =
    custom (always (Ok a))


{-| A decoder that always fails with a custom error.
-}
fail : String -> Decoder id val a
fail err =
    custom (always (Err err))


{-| -}
custom : (Value.Value val -> Result String a) -> Decoder id val a
custom func =
    parseHelp
        (\input ->
            func (Value.Value (Input.value input))
                |> Result.mapError
                    (CustomError (Input.identifier input))
        )


{-| -}
format : (String -> String) -> Decoder id val String
format func =
    Decoder
        (\input ->
            case Input.value input |> Internal.Value.toString of
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


parseValue : (Value.Value val -> Maybe a) -> Decoder id val a
parseValue func =
    parseHelp
        (\input ->
            if Input.isGroup input then
                Err (IsGroupNotInput (Input.identifier input))

            else
                Internal.Value.toString (Input.value input)
                    |> Maybe.andThen
                        (\key ->
                            Input.options input
                                |> Dict.fromList
                                |> Dict.get key
                        )
                    |> Maybe.withDefault (Input.value input)
                    |> (func << Value.Value)
                    |> Maybe.map Ok
                    |> Maybe.withDefault
                        (Err (ParseError (Input.identifier input)))
        )


parseHelp : (Input id val -> Result (Error id val) a) -> Decoder id val a
parseHelp func =
    Decoder
        (\input ->
            case func input of
                Ok a ->
                    Success input a

                Err err ->
                    failure input err
        )


failure : Input id val -> Error id val -> Partial id val a
failure input err =
    Failure (Input.setErrors [ err ] input) [ err ]


{-| Chains together decoders that depend on previous decoding results.

    -- justinmimbs/date


    import Date exposing (Date)

    dateDecoder : Decoder id val Date
    dateDecoder =
        string
            |> andThen
                (\strValue ->
                    case Date.fromString "dd.MM.yyyy" strValue of
                        Ok date ->
                            succeed date

                        Err err ->
                            fail err
                )

    result =
        Input.text [ Input.value (Value.string "07.03.1981") ]
            |> decode dateDecoder

    --> Ok (Date.fromCalendarDate 1981 Date.March 7)

-}
andThen : (a -> Decoder id val b) -> Decoder id val a -> Decoder id val b
andThen func (Decoder decoder) =
    Decoder
        (\input ->
            case decoder input of
                Success input2 a ->
                    apply (func a) input2

                Failure input2 errors ->
                    Failure input2 errors
        )


{-| Incrementally apply decoders in a pipeline fashion.


    type alias Person =
        { firstName : String
        , lastName : String
        , age : Int
        }

    type Fields
        = FirstName
        | LastName
        | Age

    form : Input Fields val
    form =
        Input.group []
            [ Input.text
                [ Input.identifier FirstName
                , Input.value (Value.string "Penny")
                ]
            , Input.text
                [ Input.identifier LastName
                , Input.value (Value.string "Rimbaud")
                ]
            , Input.int
                [ Input.identifier Age
                , Input.value (Value.string 81)
                ]
            ]

    personDecoder : Decoder Fields val Person
    personDecoder =
        Decode.succeed Person
            |> Decode.andMap (Decode.field FirstName Decode.string)
            |> Decode.andMap (Decode.field LastName Decode.string)
            |> Decode.andMap (Decode.int Age Decode.int)

    result =
        form |> decode personDecoder

    --> Ok { firstName = "Penny", lastName = "Rimbaud", age = 81 }

-}
andMap : Decoder id val a -> Decoder id val (a -> b) -> Decoder id val b
andMap a b =
    map2 (|>) a b


{-| Transforms the result of a decoder using a function.

    Input.text [ Input.value (Value.string "a string") ]
        |> decode (map String.toUpper string)
        == Ok "A STRING"

-}
map : (a -> b) -> Decoder id val a -> Decoder id val b
map func decoder =
    Decoder (mapHelp func decoder)


mapHelp : (a -> b) -> Decoder id val a -> Input id val -> Partial id val b
mapHelp func (Decoder decoder) input =
    case decoder input of
        Success input2 a ->
            Success input2 (func a)

        Failure input2 errors ->
            Failure input2 errors


{-| Combines two decoders using a function.

    Input.group []
        [ Input.text
            [ Input.identifier "FirstName"
            , Input.value (Value.string "Iris")
            ]
        , Input.text
            [ Input.identifier "LastName"
            , Input.value (Value.string "Hefets")
            ]
        ]
        |> decode
            (map2 Tuple.pair
                (field "FirstName" string)
                (field "LastName" string)
            )
        == Ok ( "Iris", "Hefets" )

-}
map2 : (a -> b -> c) -> Decoder id val a -> Decoder id val b -> Decoder id val c
map2 func a b =
    Decoder
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


    type alias Person =
        { firstName : String
        , lastName : String
        , age : Int
        }

    person : Decoder String val Person
    person =
        map3 Person
            (field "FirstName" string)
            (field "LastName" string)
            (field "Age" int)

    form : Input String val
    form =
        Input.group []
            [ Input.text
                [ Input.identifier "FirstName"
                , Input.value (Value.string "Penny")
                ]
            , Input.text
                [ Input.identifier "LastName"
                , Input.value (Value.string "Rimbaud")
                ]
            , Input.int
                [ Input.identifier "Age"
                , Input.value (Value.int 81)
                ]
            ]

    result =
        form |> decode personDecoder

    --> Ok { firstName = "Penny", lastName = "Rimbaud", age = 81 }

-}
map3 :
    (a -> b -> c -> out)
    -> Decoder id val a
    -> Decoder id val b
    -> Decoder id val c
    -> Decoder id val out
map3 func a b c =
    map2 func a b |> andMap c


{-| -}
map4 :
    (a -> b -> c -> d -> out)
    -> Decoder id val a
    -> Decoder id val b
    -> Decoder id val c
    -> Decoder id val d
    -> Decoder id val out
map4 func a b c d =
    map3 func a b c |> andMap d


{-| -}
map5 :
    (a -> b -> c -> d -> e -> out)
    -> Decoder id val a
    -> Decoder id val b
    -> Decoder id val c
    -> Decoder id val d
    -> Decoder id val e
    -> Decoder id val out
map5 func a b c d e =
    map4 func a b c d |> andMap e


{-| -}
map6 :
    (a -> b -> c -> d -> e -> f -> out)
    -> Decoder id val a
    -> Decoder id val b
    -> Decoder id val c
    -> Decoder id val d
    -> Decoder id val e
    -> Decoder id val f
    -> Decoder id val out
map6 func a b c d e f =
    map5 func a b c d e |> andMap f


{-| -}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> out)
    -> Decoder id val a
    -> Decoder id val b
    -> Decoder id val c
    -> Decoder id val d
    -> Decoder id val e
    -> Decoder id val f
    -> Decoder id val g
    -> Decoder id val out
map7 func a b c d e f g =
    map6 func a b c d e f |> andMap g


{-| -}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> out)
    -> Decoder id val a
    -> Decoder id val b
    -> Decoder id val c
    -> Decoder id val d
    -> Decoder id val e
    -> Decoder id val f
    -> Decoder id val g
    -> Decoder id val h
    -> Decoder id val out
map8 func a b c d e f g h =
    map7 func a b c d e f g |> andMap h


{-| Decodes an input using the given decoder without applying field validations
(required, min, max...).

    Input.text [ Input.value (Value.string "A string"), Input.required True ]
        |> decode string
        == Ok "A string"

    Input.text
        [ Input.value (Value.bool True)
        , Input.identifier "MyInput"
        ]
        |> decode string
        == Err [ ParseError (Just "MyInput") ]

-}
decode : Decoder id val a -> Input id val -> Result (List (Error id val)) a
decode decoder input =
    case apply decoder input of
        Success _ a ->
            Ok a

        Failure _ errors ->
            Err errors


{-| Validates and decodes an input using the given decoder.
-}
validateAndDecode :
    Decoder id val a
    -> Input id val
    -> ( Input id val, Result (List (Error id val)) a )
validateAndDecode decoder input =
    case apply (validateTree |> andThen (\() -> decoder)) input of
        Success input2 a ->
            ( input2, Ok a )

        Failure input2 errors ->
            ( input2, Err errors )


validateTree : Decoder id val ()
validateTree =
    Decoder
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
                                    Input.setErrors errors node
                        )
                        input
            in
            case Input.errors updated of
                [] ->
                    Success updated ()

                errors ->
                    Failure updated errors
        )


apply : Decoder id val a -> Input id val -> Partial id val a
apply (Decoder decoder) =
    decoder


checkRequired : Input id val -> Maybe (Error id val)
checkRequired input =
    if Input.isRequired input && Input.isBlank input then
        Just (IsBlank (Input.identifier input))

    else
        Nothing


checkInRange : Input id val -> Maybe (Error id val)
checkInRange tree =
    let
        val =
            Value.Value (Input.value tree)

        min =
            Value.Value (Input.min tree)

        max =
            Value.Value (Input.max tree)
    in
    case
        ( Internal.Value.compare (Input.value tree) (Input.min tree)
        , Internal.Value.compare (Input.value tree) (Input.max tree)
        )
    of
        ( Just LT, Just _ ) ->
            Just
                (ValueNotInRange (Input.identifier tree)
                    { value = val, min = min, max = max }
                )

        ( Just _, Just GT ) ->
            Just
                (ValueNotInRange (Input.identifier tree)
                    { value = val, min = min, max = max }
                )

        ( Just LT, Nothing ) ->
            Just (ValueTooSmall (Input.identifier tree) { value = val, min = min })

        ( Nothing, Just GT ) ->
            Just (ValueTooLarge (Input.identifier tree) { value = val, max = max })

        _ ->
            Nothing


checkOptionsProvided : Input id val -> Maybe (Error id val)
checkOptionsProvided input =
    case ( Input.inputType input, Input.options input ) of
        ( Input.Select, [] ) ->
            Just (NoOptionsProvided (Input.identifier input))

        ( Input.Radio, [] ) ->
            Just (NoOptionsProvided (Input.identifier input))

        ( Input.StrictAutocomplete, [] ) ->
            Just (NoOptionsProvided (Input.identifier input))

        _ ->
            Nothing


{-| Represents an error that occurred during decoding or validation.
-}
type Error id val
    = ValueTooLarge (Maybe id) { value : Value.Value val, max : Value.Value val }
    | ValueTooSmall (Maybe id) { value : Value.Value val, min : Value.Value val }
    | ValueNotInRange (Maybe id) { value : Value.Value val, min : Value.Value val, max : Value.Value val }
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
