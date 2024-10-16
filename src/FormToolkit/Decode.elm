module FormToolkit.Decode exposing
    ( Decoder
    , field
    , string, int, float, bool, posix, maybe, list, value, json
    , succeed, fail
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
@docs string, int, float, bool, posix, maybe, list, value, json
@docs succeed, fail


# Maps, combinators and pipeline style decoding

@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap


# Decoding

@docs decode, validateAndDecode
@docs Error, errorToFieldIdentifier

-}

import FormToolkit.Value as Value
import Internal.Input exposing (Input(..))
import Internal.Value
import Json.Decode
import Json.Encode
import RoseTree.Tree as Tree
import Time


type Partial id val a
    = Failure (Tree id val) (List (Error id val))
    | Success (Tree id val) a


{-| A decoder that takes a tree of input data and returns a decoded result or an
error if the decoding fails.
-}
type Decoder id val a
    = Decoder (Tree id val -> Partial id val a)


type alias Tree id val =
    Tree.Tree (Internal.Input.Attrs id val (Error id val))


type alias Input id val =
    Internal.Input.Input id val (Error id val)


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
                    let
                        err =
                            InputNotFound id
                    in
                    Failure (setError err tree) [ err ]
        )


fieldHelp : id -> Decoder id val a -> Tree id val -> ( Maybe (Partial id val a), List Int )
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
list : Decoder id val a -> Decoder id val (List a)
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
    Decoder id val a
    -> Tree id val
    -> ( List (Tree id val), Result (List (Error id val)) (List a) )
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
value : Decoder id val (Value.Value val)
value =
    parseValue Just


{-| Converts the entire input tree into a JSON
[Value](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode#Value).
Input [name](FormToolkit.Input#name) property will be used as the key,
if an input [name](FormToolkit.Input#name) is not present the decoder will fail.

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
json : Decoder id val Json.Decode.Value
json =
    custom (\(Input tree) -> jsonEncodeObject tree)


jsonEncodeObject : Tree id val -> Result (Error id val) Json.Encode.Value
jsonEncodeObject tree =
    jsonEncodeHelp tree [] |> Result.map Json.Encode.object


jsonEncodeHelp :
    Tree id val
    -> List ( String, Json.Decode.Value )
    -> Result (Error id val) (List ( String, Json.Decode.Value ))
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

    specialDecoder : Decoder Special
    specialDecoder =
        string
            |> andThen
                (\strValue ->
                    if strValue == "special" then
                        succeed SpecialValue

                    else
                        fail (Input.CustomError Nothing "Not special")
                )

    result =
        Input.text [ Input.value (Value.string "special") ]
            |> decode specialDecoder

    -- Ok SpecialValue

-}
succeed : a -> Decoder id val a
succeed a =
    custom (always (Ok a))


{-| A decoder that always fails with the given error.
-}
fail : Error id val -> Decoder id val a
fail error =
    custom (always (Err error))


custom : (Input id val -> Result (Error id val) a) -> Decoder id val a
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
    (Input id val -> Result (Error id val) (Value.Value val))
    -> Decoder id val a
    -> Decoder id val a
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
    (Internal.Input.Attrs id val (Error id val) -> Result (Error id val) (Value.Value val))
    -> Tree id val
    -> ( Tree id val, List (Error id val) )
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
parseValue : (Value.Value val -> Maybe a) -> Decoder id val a
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


setError : Error id val -> Tree id val -> Tree id val
setError error =
    Tree.updateValue (\input -> { input | errors = error :: input.errors })


inputFromInternal : Internal.Input.Attrs id val (Error id val) -> Input id val
inputFromInternal node =
    Input (Tree.leaf node)


{-| Chains together decoders that depend on previous decoding results.

    -- justinmimbs/date


    import Date

    dateDecoder : Decoder Date
    dateDecoder =
        string
            |> andThen
                (\strValue ->
                    case Date.fromString "dd.MM.yyyy" strValue of
                        Ok date ->
                            succeed date

                        Err err ->
                            fail (Input.CustomError Nothing err)
                )

    result =
        Input.text [ Input.value (Value.string "07.03.1981") ]
            |> decode dateDecoder

    -- Ok (Date.fromCalendarDate 1981 Date.March 7)

-}
andThen : (a -> Decoder id val b) -> Decoder id val a -> Decoder id val b
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


mapHelp : (a -> b) -> Decoder id val a -> Tree id val -> Partial id val b
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
map2 : (a -> b -> c) -> Decoder id val a -> Decoder id val b -> Decoder id val c
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
decode : Decoder id val a -> Input id val -> Result (List (Error id val)) a
decode decoder =
    validateAndDecode decoder >> Tuple.second


{-| Validates and decodes an input using the given decoder.
-}
validateAndDecode :
    Decoder id val a
    -> Input id val
    -> ( Input id val, Result (List (Error id val)) a )
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


apply : Decoder id val a -> Tree id val -> Partial id val a
apply (Decoder decoder) =
    decoder


checkRequired : Input id val -> Result (Error id val) (Value.Value val)
checkRequired (Input tree) =
    let
        input =
            Tree.value tree
    in
    if input.isRequired && Internal.Value.isBlank input.value then
        Err (IsBlank input.identifier)

    else
        Ok (Value.Value input.value)


checkInRange : Input id val -> Result (Error id val) (Value.Value val)
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
    | ParseError (Maybe id)
    | ListError (Maybe id) { index : Int, error : Error id val }
    | RepeatableHasNoName (Maybe id)
    | InputNotFound id
    | CustomError (Maybe id) String


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

        ParseError maybeId ->
            maybeId

        IsBlank maybeId ->
            maybeId

        ListError maybeId _ ->
            maybeId

        RepeatableHasNoName maybeId ->
            maybeId

        InputNotFound id ->
            Just id

        CustomError maybeId _ ->
            maybeId
