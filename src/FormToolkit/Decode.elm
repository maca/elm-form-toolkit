module FormToolkit.Decode exposing
    ( Decoder
    , field
    , string, int, float, bool, posix, maybe, list, value, json
    , succeed, fail, custom
    , map, map2, map3, map4, map5, map6, map7, map8
    , andThen, andMap
    , decode, validateAndDecode
    )

{-| This module contains a set of decoders that are useful when working with
forms. It provides functions to decode data of different types (e.g., string,
int, float, bool, posix, and lists) as well as utility functions to combine
decoders and perform decoding operations.

@docs Decoder


# Decoding functions

@docs field
@docs string, int, float, bool, posix, maybe, list, value, json
@docs succeed, fail, custom


# Maps and combinators

@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap


# Decoding

@docs decode, validateAndDecode

-}

import FormToolkit.Input as Input exposing (Error(..), Input(..))
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


{-| A decoder that takes a tree of input data and returns a decoded result
or an error if the decoding fails.
-}
type Decoder id a
    = Decoder (Tree id -> Partial id a)


type alias Tree id =
    Tree.Tree (Internal.Input.Input id (Error id))


{-| Decoder for a field with the given ID using a provided decoder.

    form : Input String
    form =
        Input.group
            []
            [ Input.text
                [ Input.label "first-name"
                , Input.identifier "name"
                , Input.value (Value.string "Juan")
                ]
            ]
    decode (field "first-name" string) form == Ok "Juan"

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


{-| TODO
-}
string : Decoder id String
string =
    parseValue Value.toString


{-| TODO
-}
int : Decoder id Int
int =
    parseValue Value.toInt


{-| TODO
-}
float : Decoder id Float
float =
    parseValue Value.toFloat


{-| TODO
-}
bool : Decoder id Bool
bool =
    parseValue Value.toBool


{-| TODO
-}
posix : Decoder id Time.Posix
posix =
    parseValue Value.toPosix


{-| TODO
-}
value : Decoder id Value.Value
value =
    parseValue Just


{-| TODO
-}
json : Decoder id Json.Decode.Value
json =
    custom
        (\(Input.Input tree) ->
            Ok (Json.Encode.object (encodeHelp tree []))
        )


encodeHelp : Tree id -> List ( String, Json.Decode.Value ) -> List ( String, Json.Decode.Value )
encodeHelp tree acc =
    let
        input =
            Tree.value tree

        children =
            Tree.children tree
    in
    case input.inputType of
        Internal.Input.Group ->
            List.foldl encodeHelp acc children

        Internal.Input.Repeatable _ ->
            ( input.name
            , Json.Encode.list (\e -> Json.Encode.object (encodeHelp e []))
                children
            )
                :: acc

        _ ->
            ( input.name, Internal.Value.encode input.value ) :: acc


{-| TODO
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


{-| TODO
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


{-| TODO
-}
succeed : a -> Decoder id a
succeed a =
    custom (always (Ok a))


{-| TODO
-}
fail : Error id -> Decoder id a
fail error =
    custom (always (Err error))


setError : Error id -> Tree id -> Tree id
setError error =
    Tree.updateValue (\input -> { input | errors = error :: input.errors })


{-| TODO
-}
parseValue : (Value.Value -> Maybe a) -> Decoder id a
parseValue func =
    custom
        (\(Input.Input tree) ->
            let
                input =
                    Tree.value tree
            in
            .value input
                |> Value.Value
                |> func
                |> Result.fromMaybe (ParseError input.identifier)
        )


{-| TODO
-}
custom : (Input id -> Result (Error id) a) -> Decoder id a
custom func =
    Decoder
        (\tree ->
            case func (Input.Input tree) of
                Ok a ->
                    Success tree a

                Err err ->
                    Failure (setError err tree) [ err ]
        )
        |> validate checkRequired
        |> validate checkInRange


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


inputFromInternal : Internal.Input.Input id (Error id) -> Input id
inputFromInternal node =
    Input (Tree.leaf node)


{-| TODO
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


{-| TODO
-}
andMap : Decoder id a -> Decoder id (a -> b) -> Decoder id b
andMap a b =
    map2 (|>) a b


{-| TODO
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


{-| TODO
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


{-| TODO
-}
map3 :
    (a -> b -> c -> out)
    -> Decoder id a
    -> Decoder id b
    -> Decoder id c
    -> Decoder id out
map3 func a b c =
    map2 func a b |> andMap c


{-| TODO
-}
map4 :
    (a -> b -> c -> d -> out)
    -> Decoder id a
    -> Decoder id b
    -> Decoder id c
    -> Decoder id d
    -> Decoder id out
map4 func a b c d =
    map3 func a b c |> andMap d


{-| TODO
-}
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


{-| TODO
-}
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


{-| TODO
-}
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


{-| TODO
-}
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


{-| TODO
-}
decode : Decoder id a -> Input id -> Result (List (Error id)) a
decode decoder =
    validateAndDecode decoder >> Tuple.second


{-| TODO
-}
validateAndDecode :
    Decoder id a
    -> Input id
    -> ( Input id, Result (List (Error id)) a )
validateAndDecode decoder (Input.Input tree) =
    case apply decoder tree of
        Success tree2 a ->
            ( Input.Input tree2, Ok a )

        Failure tree2 errors ->
            ( Input.Input tree2, Err errors )


apply : Decoder id a -> Tree id -> Partial id a
apply (Decoder decoder) =
    decoder


checkRequired : Input id -> Result (Error id) Value.Value
checkRequired (Input.Input tree) =
    let
        input =
            Tree.value tree
    in
    if input.isRequired && Internal.Value.isBlank input.value then
        Err (IsBlank input.identifier)

    else
        Ok (Value.Value input.value)


checkInRange : Input id -> Result (Error id) Value.Value
checkInRange (Input.Input tree) =
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
