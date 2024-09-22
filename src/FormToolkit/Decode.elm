module FormToolkit.Decode exposing
    ( Decoder
    , field
    , string, int, float, bool, posix, maybe, list, value, json
    , succeed, fail
    , map, map2, map3, map4, map5, map6, map7, map8
    , andThen, andMap
    , decode, validateAndDecode
    )

{-| Map the values of an input or group of inputs to any shape you want, if you
know `Json.Decode` you know how to use this module ;)

@docs Decoder


# Decoding functions

@docs field
@docs string, int, float, bool, posix, maybe, list, value, json
@docs succeed, fail


# Maps and combinators

@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap


# Decoding

@docs decode, validateAndDecode

-}

import FormToolkit.Input exposing (Error(..), Input(..))
import FormToolkit.Value as Value
import Internal.Input
import Internal.Value
import Json.Decode
import Json.Encode
import RoseTree.Tree as Tree
import Time


type Partial id a
    = Failure (Tree id) (List (Error id))
    | Success (Tree id) a


{-| A decoder that takes a tree of input data and returns a decoded result or an
error if the decoding fails.
-}
type Decoder id a
    = Decoder (Tree id -> Partial id a)


type alias Tree id =
    Tree.Tree (Internal.Input.Input id (Error id))


{-| Decoder for a field with the given identifier using a provided decoder.


    type Fields
        = FirstName
        | LastName

    form : Input Fields
    form =
        Input.group []
            [ Input.text
                [ Input.label "First name"
                , Input.identifier FirstName
                , Input.value (Value.string "Juan")
                ]
            , Input.text
                [ Input.label "Last name"
                , Input.identifier LastName
                , Input.value (Value.string "Perez")
                ]
            ]

    decoded =
        form |> decode (field FirstName string)

    -- Ok "Juan"

-}
field : id -> Decoder id a -> Decoder id a
field id decoder =
    Decoder
        (\tree ->
            case fieldHelp id decoder tree of
                ( Just (Success node a), path ) ->
                    Success (Tree.replaceAt path node tree) a

                ( Just (Failure node errors), path ) ->
                    Failure (Tree.replaceAt path node tree) errors

                ( Nothing, _ ) ->
                    let
                        err =
                            InputNotFound id
                    in
                    Failure (setError err tree) [ err ]
        )


fieldHelp : id -> Decoder id a -> Tree id -> ( Maybe (Partial id a), List Int )
fieldHelp id decoder tree =
    Tree.foldWithPath
        (\path node acc ->
            if .identifier (Tree.value node) == Just id then
                ( Just (apply decoder node), path )

            else
                acc
        )
        ( Nothing, [] )
        tree


{-| Decodes the input value as a `String`.

    Input.text [ Input.value (Value.string "A string") ]
        |> decode string
        == Ok "A string"

-}
string : Decoder id String
string =
    parseValue Value.toString


{-| Decodes the input value as an `Int`.

    Input.text [ Input.value (Value.int 10) ]
        |> decode int
        == Ok 10

-}
int : Decoder id Int
int =
    parseValue Value.toInt


{-| Decodes the input value as a `Float`.

    Input.text [ Input.value (Value.float 10.5) ]
        |> decode float
        == Ok 10.5

-}
float : Decoder id Float
float =
    parseValue Value.toFloat


{-| Decodes the input value as a `Bool`.

    Input.text [ Input.value (Value.bool True) ]
        |> decode bool
        == Ok True

-}
bool : Decoder id Bool
bool =
    parseValue Value.toBool


{-| Decodes the input value as a
[Time.Posix](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix).

    Input.date [ Input.value (Value.time (Time.millisToPosix 0)) ]
        |> (posix |> map (Time.toYear Time.utc))
        == Ok 1970

-}
posix : Decoder id Time.Posix
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
maybe : Decoder id a -> Decoder id (Maybe a)
maybe decoder =
    Decoder
        (\tree ->
            if Internal.Input.isBlank (Tree.value tree) then
                Success tree Nothing

            else
                mapHelp Just decoder tree
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
list : Decoder id a -> Decoder id (List a)
list decoder =
    Decoder
        (\tree ->
            let
                ( children, result ) =
                    listHelp decoder tree

                tree2 =
                    Tree.branch (Tree.value tree) children
            in
            case result of
                Ok elements ->
                    Success tree2 elements

                Err errors ->
                    Failure tree2 errors
        )


listHelp :
    Decoder id a
    -> Tree id
    -> ( List (Tree id), Result (List (Error id)) (List a) )
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
                                Err (errors ++ errors2)
                        )
            )
            ( [], Ok [] )


{-| Returns the raw value of the input without any decoding.

    Input.text [ Input.value (Value.string "A string") ]
        |> decode value
        == Value.string "A string"

-}
value : Decoder id Value.Value
value =
    parseValue Just


{-| Converts the entire input tree into a JSON
[Value](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode#Value).
Input [name](FormToolkit.Input#name) property will be used as the key,
[name](FormToolkit.Input#name) is required.

Usefull if you just one to forward the form values to a backend.

    Input.group []
        [ Input.text
            [ Input.label "First name"
            , Input.name "first-name"
            , Input.value (Value.string "Juan")
            ]
        , Input.text
            [ Input.label "Last name"
            , Input.name "last-name"
            , Input.value (Value.string "Perez")
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
        == Ok "{\"first-name\":\"Juan\",\"last-name\":\"Perez\",\"fruits\":[{\"fruit\":\"mango\"},{\"fruit\":\"banana\"}]}"

-}
json : Decoder id Json.Decode.Value
json =
    custom (\(Input tree) -> jsonEncodeObject tree)


jsonEncodeObject : Tree id -> Result (Error id) Json.Encode.Value
jsonEncodeObject tree =
    jsonEncodeHelp tree [] |> Result.map Json.Encode.object


jsonEncodeHelp :
    Tree id
    -> List ( String, Json.Decode.Value )
    -> Result (Error id) (List ( String, Json.Decode.Value ))
jsonEncodeHelp tree acc =
    let
        input =
            Tree.value tree

        accumulate jsonValue =
            case input.name of
                Just name ->
                    Ok (( name, jsonValue ) :: acc)

                Nothing ->
                    Err (RepeatableHasNoName input.identifier)
    in
    case input.inputType of
        Internal.Input.Group ->
            case input.name of
                Nothing ->
                    Tree.children tree
                        |> List.foldr (\e -> Result.andThen (jsonEncodeHelp e))
                            (Ok acc)

                _ ->
                    Tree.children tree
                        |> List.foldr (\e -> Result.andThen (jsonEncodeHelp e))
                            (Ok [])
                        |> Result.map Json.Encode.object
                        |> Result.andThen accumulate

        Internal.Input.Repeatable _ ->
            Tree.children tree
                |> List.foldr (\e -> Result.map2 (::) (jsonEncodeObject e)) (Ok [])
                |> Result.map (Json.Encode.list identity)
                |> Result.andThen accumulate

        _ ->
            case input.name of
                Just name ->
                    Ok (( name, Internal.Value.encode input.value ) :: acc)

                Nothing ->
                    Ok acc


{-| A decoder that always succeeds with the given value, use for buiding
decoding pipelines with [andMap](#andMap), or to chain decoders with
[andThen](#andThen).


    type SpecialValue
        = SpecialValue

    result =
        Input.text [ Input.value (Value.string "special") ]
            |> decode
                (string
                    |> andThen
                        (\strValue ->
                            if strValue == "special" then
                                succeed SpecialValue

                            else
                                fail (Input.CustomError "Not special")
                        )
                )

    -- Ok SpecialValue

-}
succeed : a -> Decoder id a
succeed a =
    custom (always (Ok a))


{-| A decoder that always fails with the given error.
-}
fail : Error id -> Decoder id a
fail error =
    custom (always (Err error))


custom : (Input id -> Result (Error id) a) -> Decoder id a
custom func =
    Decoder
        (\tree ->
            case func (Input tree) of
                Ok a ->
                    Success tree a

                Err err ->
                    Failure (setError err tree) [ err ]
        )


validate :
    (Input id -> Result (Error id) Value.Value)
    -> Decoder id a
    -> Decoder id a
validate func decoder =
    Decoder
        (\tree ->
            case validateHelp (func << inputFromInternal) tree of
                ( tree2, [] ) ->
                    Success tree2 ()

                ( tree2, errors ) ->
                    Failure tree2 errors
        )
        |> andThen (\() -> decoder)


validateHelp :
    (Internal.Input.Input id (Error id) -> Result (Error id) Value.Value)
    -> Tree id
    -> ( Tree id, List (Error id) )
validateHelp func tree =
    let
        treeOfTuples =
            Tree.mapValues
                (\input ->
                    case func input of
                        Ok (Value.Value val) ->
                            ( { input | value = val }
                            , Nothing
                            )

                        Err error ->
                            ( { input | errors = error :: input.errors }
                            , Just error
                            )
                )
                tree
    in
    ( Tree.mapValues Tuple.first treeOfTuples
    , Tree.foldr
        (\node acc ->
            case Tree.value node |> Tuple.second of
                Just e ->
                    e :: acc

                Nothing ->
                    acc
        )
        []
        treeOfTuples
    )


{-| Decodes the input value using a custom parsing function.
-}
parseValue : (Value.Value -> Maybe a) -> Decoder id a
parseValue func =
    custom
        (\(Input tree) ->
            let
                input =
                    Tree.value tree
            in
            .value input
                |> Value.Value
                |> func
                |> Result.fromMaybe (ParseError input.identifier)
        )


setError : Error id -> Tree id -> Tree id
setError error =
    Tree.updateValue (\input -> { input | errors = error :: input.errors })


inputFromInternal : Internal.Input.Input id (Error id) -> Input id
inputFromInternal node =
    Input (Tree.leaf node)


{-| Chains together decoders that depend on previous decoding results.

    import Date


    -- justinmimbs/date
    result =
        Input.text [ Input.value (Value.string "07.03.1981") ]
            |> decode
                (string
                    |> andThen
                        (Date.fromString "dd.MM.yyyy"
                            >> Result.withDefault
                                (fail (Input.CustomError "Not a date"))
                        )
                )

    -- Ok (Date.fromCalendarDate 1981 Date.March 7)

-}
andThen : (a -> Decoder id b) -> Decoder id a -> Decoder id b
andThen func (Decoder decoder) =
    Decoder
        (\tree ->
            case decoder tree of
                Success tree2 a ->
                    apply (func a) tree2

                Failure tree2 errors ->
                    Failure tree2 errors
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

    form : Input Fields
    form =
        Input.group []
            [ Input.text
                [ Input.identifier FirstName
                , Input.value (Value.string "Juan")
                ]
            , Input.text
                [ Input.identifier LastName
                , Input.value (Value.string "Perez")
                ]
            , Input.int
                [ Input.identifier Age
                , Input.value (Value.string 42)
                ]
            ]

    personDecoder : Decoder Person
    personDecoder =
        Decode.succeed Person
            |> Decode.andMap (Decode.field FirstName Decode.string)
            |> Decode.andMap (Decode.field LastName Decode.string)
            |> Decode.andMap (Decode.int Age Decode.int)

    result =
        form |> decode personDecoder

    -- Ok { firstName = "Juan" , lastName = "Pérez" , age = 42 }

-}
andMap : Decoder id a -> Decoder id (a -> b) -> Decoder id b
andMap a b =
    map2 (|>) a b


{-| Transforms the result of a decoder using a function.

    Input.text [ Input.value (Value.string "a string") ]
        |> decode (map String.toUpper string)
        == Ok "A STRING"

-}
map : (a -> b) -> Decoder id a -> Decoder id b
map func decoder =
    Decoder (mapHelp func decoder)


mapHelp : (a -> b) -> Decoder id a -> Tree id -> Partial id b
mapHelp func (Decoder decoder) tree =
    case decoder tree of
        Success tree2 a ->
            Success tree2 (func a)

        Failure tree2 errors ->
            Failure tree2 errors


{-| Combines two decoders using a function.

    Input.group []
        [ Input.text
            [ Input.identifier "FirstName"
            , Input.value (Value.string "Juan")
            ]
        , Input.text
            [ Input.identifier "LastName"
            , Input.value (Value.string "Pérez")
            ]
        ]
        |> decode
            (map2 Tuple.pair
                (field "FirstName" string)
                (field "LastName" string)
            )
        == Ok ( "Juan", "Pérez" )

-}
map2 : (a -> b -> c) -> Decoder id a -> Decoder id b -> Decoder id c
map2 func a b =
    Decoder
        (\tree ->
            case apply a tree of
                Success tree2 res ->
                    case apply b tree2 of
                        Success tree3 res2 ->
                            Success tree3 (func res res2)

                        Failure tree3 errors ->
                            Failure tree3 errors

                Failure tree2 errors ->
                    case apply b tree2 of
                        Success tree3 _ ->
                            Failure tree3 errors

                        Failure tree3 errors2 ->
                            Failure tree3 (errors ++ errors2)
        )


{-| Combines three decoders using a function.


    type alias Person =
        { firstName : String
        , lastName : String
        , age : Int
        }

    person : Decoder Person
    person =
        map3 Person
            (field "FirstName" string)
            (field "LastName" string)
            (field "Age" int)

    form : Input String
    form =
        Input.group []
            [ Input.text
                [ Input.identifier "FirstName"
                , Input.value (Value.string "Juan")
                ]
            , Input.text
                [ Input.identifier "LastName"
                , Input.value (Value.string "Pérez")
                ]
            , Input.int
                [ Input.identifier "Age"
                , Input.value (Value.int 42)
                ]
            ]

    result =
        form |> decode personDecoder

    -- Ok { firstName = "Juan", lastName = "Pérez", age = 42 }

-}
map3 :
    (a -> b -> c -> out)
    -> Decoder id a
    -> Decoder id b
    -> Decoder id c
    -> Decoder id out
map3 func a b c =
    map2 func a b |> andMap c


{-| -}
map4 :
    (a -> b -> c -> d -> out)
    -> Decoder id a
    -> Decoder id b
    -> Decoder id c
    -> Decoder id d
    -> Decoder id out
map4 func a b c d =
    map3 func a b c |> andMap d


{-| -}
map5 :
    (a -> b -> c -> d -> e -> out)
    -> Decoder id a
    -> Decoder id b
    -> Decoder id c
    -> Decoder id d
    -> Decoder id e
    -> Decoder id out
map5 func a b c d e =
    map4 func a b c d |> andMap e


{-| -}
map6 :
    (a -> b -> c -> d -> e -> f -> out)
    -> Decoder id a
    -> Decoder id b
    -> Decoder id c
    -> Decoder id d
    -> Decoder id e
    -> Decoder id f
    -> Decoder id out
map6 func a b c d e f =
    map5 func a b c d e |> andMap f


{-| -}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> out)
    -> Decoder id a
    -> Decoder id b
    -> Decoder id c
    -> Decoder id d
    -> Decoder id e
    -> Decoder id f
    -> Decoder id g
    -> Decoder id out
map7 func a b c d e f g =
    map6 func a b c d e f |> andMap g


{-| -}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> out)
    -> Decoder id a
    -> Decoder id b
    -> Decoder id c
    -> Decoder id d
    -> Decoder id e
    -> Decoder id f
    -> Decoder id g
    -> Decoder id h
    -> Decoder id out
map8 func a b c d e f g h =
    map7 func a b c d e f g |> andMap h


{-| Decodes an input using the given decoder.

    Input.text [ Input.value (Value.string "A string") ]
        |> decode string
        == Ok "A string"

    Input.text
        [ Input.value (Value.bool True)
        , Input.identifier "MyInput"
        ]
        |> decode string
        == Err [ ParseError (Just "MyInput") ]

-}
decode : Decoder id a -> Input id -> Result (List (Error id)) a
decode decoder =
    validateAndDecode decoder >> Tuple.second


{-| Validates and decodes an input using the given decoder.
-}
validateAndDecode :
    Decoder id a
    -> Input id
    -> ( Input id, Result (List (Error id)) a )
validateAndDecode decoder (Input tree) =
    case
        apply
            (decoder
                |> validate checkRequired
                |> validate checkInRange
            )
            tree
    of
        Success tree2 a ->
            ( Input tree2, Ok a )

        Failure tree2 errors ->
            ( Input tree2, Err errors )


apply : Decoder id a -> Tree id -> Partial id a
apply (Decoder decoder) =
    decoder


checkRequired : Input id -> Result (Error id) Value.Value
checkRequired (Input tree) =
    let
        input =
            Tree.value tree
    in
    if input.isRequired && Internal.Value.isBlank input.value then
        Err (IsBlank input.identifier)

    else
        Ok (Value.Value input.value)


checkInRange : Input id -> Result (Error id) Value.Value
checkInRange (Input tree) =
    let
        input =
            Tree.value tree

        actual =
            Value.Value input.value
    in
    case
        ( Internal.Value.compare input.value input.min
        , Internal.Value.compare input.value input.max
        )
    of
        ( Just LT, Just _ ) ->
            Err
                (ValueNotInRange input.identifier
                    { value = actual
                    , min = Value.Value input.min
                    , max = Value.Value input.max
                    }
                )

        ( Just _, Just GT ) ->
            Err
                (ValueNotInRange input.identifier
                    { value = actual
                    , min = Value.Value input.min
                    , max = Value.Value input.max
                    }
                )

        ( Just LT, Nothing ) ->
            Err
                (ValueTooSmall input.identifier
                    { value = actual
                    , min = Value.Value input.min
                    }
                )

        ( Nothing, Just GT ) ->
            Err
                (ValueTooLarge input.identifier
                    { value = actual
                    , max = Value.Value input.max
                    }
                )

        _ ->
            Ok actual
