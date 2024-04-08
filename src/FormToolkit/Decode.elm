module FormToolkit.Decode exposing
    ( Decoder
    , field
    , string, int, float, bool, posix, maybe, list
    , custom, value
    , succeed
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
@docs custom, value

@docs succeed


# Maps and combinators

@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap


# Decoding

@docs decode

-}

import FormToolkit.Error as Error
import FormToolkit.Form exposing (Form(..))
import FormToolkit.Value as Value exposing (Value)
import Internal.Input as Input exposing (Input)
import Internal.Tree as Tree exposing (Tree)
import Time


{-| Represents an error that occurred during decoding.
-}
type Error id
    = InputError (Maybe id) Error.Error
    | ListError Int (Error id)
    | InputNotFound id


{-| A decoder that takes a tree of input data and returns a decoded result
or an error if the decoding fails.
-}
type alias Decoder id a =
    Tree (Input id) -> Result (Error id) a


{-| Decoder for a field with the given ID using a provided decoder.

    form : Form String
    form =
        Form.init
            [ Input.text
                [ Input.label "first-name"
                , Input.identifier "name"
                , Input.value (Value.string "Juan")
                ]
            ]

    decode (field "first-name" string) form == Ok "Juan"

-}
field : id -> Decoder id a -> Decoder id a
field id decoder tree =
    tree
        |> Tree.find (Tree.value >> .identifier >> (==) (Just id))
        |> Maybe.map decoder
        |> Maybe.withDefault (Err (InputNotFound id))


{-| TODO
-}
string : Decoder id String
string tree =
    value Value.toString tree


{-| TODO
-}
int : Decoder id Int
int tree =
    value Value.toInt tree


{-| TODO
-}
float : Decoder id Float
float tree =
    value Value.toFloat tree


{-| TODO
-}
bool : Decoder id Bool
bool tree =
    value Value.toBool tree


{-| TODO
-}
posix : Decoder id Time.Posix
posix tree =
    value Value.toPosix tree


{-| TODO
-}
maybe : Decoder id a -> Decoder id (Maybe a)
maybe decoder tree =
    if Input.isBlank (Tree.value tree) then
        succeed Nothing tree

    else
        Result.map Just (decoder tree)


{-| TODO
-}
list : Decoder id a -> Decoder id (List a)
list decoder tree =
    (Tree.children tree
        |> List.foldl
            (\e ( prev, i ) ->
                ( map2 (::)
                    (\_ -> Result.mapError (ListError i) (decoder e))
                    prev
                , i + 1
                )
            )
            ( succeed [], 0 )
        |> Tuple.first
        |> map List.reverse
    )
        tree


{-| TODO
-}
value : (Value -> Result Error.Error a) -> Decoder id a
value func =
    custom (Tree.value >> Input.check >> Result.andThen func)


{-| TODO
-}
custom : (Tree (Input id) -> Result Error.Error a) -> Decoder id a
custom func tree =
    Result.mapError (InputError (.identifier (Tree.value tree))) (func tree)


{-| TODO
-}
succeed : a -> Decoder id a
succeed a _ =
    Ok a


{-| TODO
-}
andThen : (a -> Decoder id b) -> Decoder id a -> Decoder id b
andThen callback decoder tree =
    Result.andThen (\res -> callback res tree) (decoder tree)


{-| TODO
-}
andMap : Decoder id a -> Decoder id (a -> b) -> Decoder id b
andMap decoder partial tree =
    Result.map2 (|>) (decoder tree) (partial tree)


{-| TODO
-}
map : (a -> b) -> Decoder id a -> Decoder id b
map func decoder tree =
    Result.map func (decoder tree)


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
decode : Decoder id a -> Form id -> Result (Error id) a
decode decoder (Form root) =
    decoder root
