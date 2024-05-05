module FormToolkit.Decode exposing
    ( Decoder
    , field
    , string, int, float, bool, posix, maybe, list, value
    , succeed, fail, custom
    , map, map2, map3, map4, map5, map6, map7, map8
    , andThen, andMap
    , decode
    , Error(..)
    )

{-| This module contains a set of decoders that are useful when working with
forms. It provides functions to decode data of different types (e.g., string,
int, float, bool, posix, and lists) as well as utility functions to combine
decoders and perform decoding operations.

@docs Decoder


# Decoding functions

@docs field
@docs string, int, float, bool, posix, maybe, list, value
@docs succeed, fail, custom


# Maps and combinators

@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap


# Decoding

@docs decode


# Errors

@docs Error

-}

import FormToolkit.Input as Input exposing (Input(..))
import FormToolkit.Value as Value
import Internal.Input
import Internal.Value
import RoseTree.Tree as Tree
import Time


type Partial id a
    = Failure (Tree id)
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

                ( Just (Failure node), path ) ->
                    Failure (Tree.replaceAt path node tree)

                ( Nothing, _ ) ->
                    Failure (setError (InputNotFound id) tree)
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

                Err () ->
                    Failure tree2
        )


listHelp : Decoder id a -> Tree id -> ( List (Tree id), Result () (List a) )
listHelp decoder =
    Tree.children
        >> List.foldr
            (\node ( nodes, result ) ->
                case apply decoder node of
                    Success node2 a ->
                        ( node2 :: nodes
                        , Result.map2 (::) (Ok a) result
                        )

                    Failure node2 ->
                        ( node2 :: nodes, Err () )
            )
            ( [], Ok [] )


{-| TODO
-}
succeed : a -> Decoder id a
succeed a =
    Decoder (\tree -> Success tree a)


{-| TODO
-}
fail : Error id -> Decoder id a
fail error =
    Decoder (Failure << setError error)


setError : Error id -> Tree id -> Tree id
setError error =
    Tree.updateValue (\input -> { input | errors = error :: input.errors })


setValue : Value.Value -> Tree id -> Tree id
setValue (Value.Value val) =
    Tree.updateValue (\input -> { input | value = val })


{-| TODO
-}
parseValue : (Value.Value -> Maybe a) -> Decoder id a
parseValue func =
    custom
        (\(Input.Input node) ->
            node
                |> Tree.value
                |> .value
                |> Value.Value
                |> func
                |> Result.fromMaybe ParseError
        )
        |> validate checkRequired
        |> validate checkInRange


{-| TODO
-}
custom : (Input id (Error id) -> Result (Error id) a) -> Decoder id a
custom func =
    Decoder
        (\tree ->
            case func (Input.Input tree) of
                Ok a ->
                    Success tree a

                Err err ->
                    Failure (setError err tree)
        )


validate :
    (Input id (Error id) -> Result (Error id) Value.Value)
    -> Decoder id a
    -> Decoder id a
validate func decoder =
    Decoder
        (\tree ->
            case func (Input.Input tree) of
                Ok val ->
                    Success (setValue val tree) ()

                Err err ->
                    Failure (setError err tree)
        )
        |> andThen (\() -> decoder)


{-| TODO
-}
andThen : (a -> Decoder id b) -> Decoder id a -> Decoder id b
andThen func (Decoder decoder) =
    Decoder
        (\tree ->
            case decoder tree of
                Success tree2 a ->
                    apply (func a) tree2

                Failure tree2 ->
                    Failure tree2
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

        Failure tree2 ->
            Failure tree2


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

                        Failure tree3 ->
                            Failure tree3

                Failure tree2 ->
                    case apply b tree2 of
                        Success tree3 _ ->
                            Failure tree3

                        Failure tree3 ->
                            Failure tree3
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
decode : Decoder id a -> Input id (Error id) -> ( Input id (Error id), Result () a )
decode decoder (Input.Input tree) =
    case apply decoder tree of
        Success tree2 a ->
            ( Input.Input tree2, Ok a )

        Failure tree2 ->
            ( Input.Input tree2, Err () )


apply : Decoder id a -> Tree id -> Partial id a
apply (Decoder decoder) =
    decoder


{-| Represents an error that occurred during decoding.
-}
type Error id
    = ValueTooLarge { value : Value.Value, max : Value.Value }
    | ValueTooSmall { value : Value.Value, min : Value.Value }
    | ValueNotInRange
        { value : Value.Value
        , min : Value.Value
        , max : Value.Value
        }
    | IsBlank
    | ParseError
    | InputNotFound id


checkRequired : Input id (Error id) -> Result (Error id) Value.Value
checkRequired (Input.Input tree) =
    let
        input =
            Tree.value tree
    in
    if input.isRequired && Internal.Value.isBlank input.value then
        Err IsBlank

    else
        Ok (Value.Value input.value)


checkInRange : Input id (Error id) -> Result (Error id) Value.Value
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
                (ValueNotInRange
                    { value = actual
                    , min = Value.Value input.min
                    , max = Value.Value input.max
                    }
                )

        ( Just _, Just GT ) ->
            Err
                (ValueNotInRange
                    { value = actual
                    , min = Value.Value input.min
                    , max = Value.Value input.max
                    }
                )

        ( Just LT, Nothing ) ->
            Err
                (ValueTooSmall
                    { value = actual
                    , min = Value.Value input.min
                    }
                )

        ( Nothing, Just GT ) ->
            Err
                (ValueTooLarge
                    { value = actual
                    , max = Value.Value input.max
                    }
                )

        _ ->
            Ok actual



-- toJSON : Input id (Error id) -> Encode.Value
-- toJSON input =
--     Encode.object (encodeHelp input [])
-- encodeHelp : Input id (Error id) -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
-- encodeHelp inputElement acc =
--     let
--         tree =
--             toTree inputElement
--         input =
--             Tree.value tree
--         children =
--             Tree.children tree
--     in
--     case input.inputType of
--         Internal.Group ->
--             List.foldl (encodeHelp << Input) acc children
--         Internal.Repeatable _ ->
--             ( input.name
--             , Encode.list
--                 (\e -> Encode.object (encodeHelp (Input e) []))
--                 children
--             )
--                 :: acc
--         _ ->
--             ( input.name, Internal.Value.encode input.value ) :: acc
