module FormToolkit.Decode exposing
    ( Decoder
    , field
    , string, int, float, bool, posix, maybe, list
    , succeed, fail, custom
    , map, map2, map3, map4, map5, map6, map7, map8
    , andThen, andMap
    , decode
    )

{-| This module contains a set of decoders that are useful when working with
forms. It provides functions to decode data of different types (e.g., string,
int, float, bool, posix, and lists) as well as utility functions to combine
decoders and perform decoding operations.

@docs Decoder


# Decoding functions

@docs field
@docs string, int, float, bool, posix, maybe, list
@docs succeed, fail, custom


# Maps and combinators

@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap


# Decoding

@docs decode

-}

import FormToolkit.Input as Input exposing (Input)
import FormToolkit.Value as Value
import Internal.Input
import Internal.Value
import RoseTree.Tree as Tree exposing (Tree)
import Time


{-| A decoder that takes a tree of input data and returns a decoded result
or an error if the decoding fails.
-}
type Decoder id a
    = Decoder (Tree (Internal.Input.Input id (Input.Error id)) -> Partial id a)


type alias Partial id a =
    Result (Input.Error id) a


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
field id (Decoder decoder) =
    Decoder
        (\tree ->
            tree
                |> Tree.findl
                    (Tree.value >> .identifier >> (==) (Just id))
                |> Maybe.map decoder
                |> Maybe.withDefault (Err (Input.InputNotFound id))
        )


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
maybe (Decoder decoder) =
    Decoder
        (\tree ->
            if Internal.Input.isBlank (Tree.value tree) then
                Ok Nothing

            else
                Result.map Just (decoder tree)
        )


{-| TODO
-}
list : Decoder id a -> Decoder id (List a)
list (Decoder decoder) =
    Decoder
        (\tree ->
            Tree.children tree
                |> List.foldl
                    (\e ( prev, i ) ->
                        ( Result.map2 (::)
                            (Result.mapError
                                (Input.ListError i)
                                (decoder e)
                            )
                            prev
                        , i + 1
                        )
                    )
                    ( Ok [], 0 )
                |> Tuple.first
                |> Result.map List.reverse
        )


{-| TODO
-}
succeed : a -> Decoder id a
succeed a =
    Decoder (\_ -> Ok a)


{-| TODO
-}
fail : Input.Error id -> Decoder id a
fail error =
    Decoder (\_ -> Err error)


{-| TODO
-}
parseValue : (Value.Value -> Maybe a) -> Decoder id a
parseValue func =
    custom
        (\tree ->
            Input.toTree tree
                |> Tree.value
                |> .value
                |> Value.Value
                |> func
                |> Result.fromMaybe (Input.ParseError Nothing)
        )
        |> validate checkRequired


{-| TODO
-}
custom : (Input id -> Result (Input.Error id) a) -> Decoder id a
custom func =
    Decoder (\tree -> func (Input.fromTree tree))


validate :
    (Input id -> Result (Input.Error id) Value.Value)
    -> Decoder id a
    -> Decoder id a
validate func decoder =
    Decoder
        (\tree ->
            case func (Input.fromTree tree) of
                Ok val ->
                    Ok val

                Err err ->
                    Err err
        )
        |> andThen (always decoder)


{-| TODO
-}
andThen : (a -> Decoder id b) -> Decoder id a -> Decoder id b
andThen func (Decoder decoder) =
    Decoder
        (\tree ->
            Result.andThen
                (\res ->
                    let
                        (Decoder partial) =
                            func res
                    in
                    partial tree
                )
                (decoder tree)
        )


{-| TODO
-}
andMap : Decoder id a -> Decoder id (a -> b) -> Decoder id b
andMap (Decoder decoder) (Decoder partial) =
    Decoder (\tree -> Result.map2 (|>) (decoder tree) (partial tree))


{-| TODO
-}
map : (a -> b) -> Decoder id a -> Decoder id b
map func (Decoder decoder) =
    Decoder (\tree -> Result.map func (decoder tree))


{-| TODO
-}
map2 : (a -> b -> c) -> Decoder id a -> Decoder id b -> Decoder id c
map2 func a b =
    map func a |> andMap b


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
decode : Decoder id a -> Input id -> Result (Input.Error id) a
decode (Decoder decoder) input =
    decoder (Input.toTree input)



-- {-| TODO
-- -}
-- validate : Tree (Internal.Input id error) -> Tree (Internal.Input id error)
-- validate =
--     Tree.updateValue
--         (\node ->
--             case check node of
--                 Ok _ ->
--                     { node | status = Internal.Valid }
--                 Err err ->
--                     { node | status = Internal.WithError err }
--         )


checkRequired : Input id -> Result (Input.Error id) Value.Value
checkRequired (Input.Input tree) =
    let
        input =
            Tree.value tree
    in
    if input.isRequired && Internal.Value.isBlank input.value then
        Err Input.IsBlank

    else
        Ok (Value.Value input.value)


checkInRange : Input id -> Result (Input.Error id) Value.Value
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
                (Input.NotInRange
                    { value = actual
                    , min = Value.Value input.min
                    , max = Value.Value input.max
                    }
                )

        ( Just _, Just GT ) ->
            Err
                (Input.NotInRange
                    { value = actual
                    , min = Value.Value input.min
                    , max = Value.Value input.max
                    }
                )

        ( Just LT, Nothing ) ->
            Err
                (Input.TooSmall
                    { value = actual
                    , min = Value.Value input.min
                    }
                )

        ( Nothing, Just GT ) ->
            Err
                (Input.TooLarge
                    { value = actual
                    , max = Value.Value input.max
                    }
                )

        _ ->
            Ok actual
