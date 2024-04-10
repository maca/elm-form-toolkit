module FormToolkit.Decode exposing
    ( Decoder
    , field
    , string, int, float, bool, posix, maybe, list
    , succeed
    , custom
    , map, map2, map3, map4, map5, map6, map7, map8
    , andThen, andMap
    , Error(..), decode
    )

{-| This module contains a set of decoders that are useful when working with
forms. It provides functions to decode data of different types (e.g., string,
int, float, bool, posix, and lists) as well as utility functions to combine
decoders and perform decoding operations.

@docs Decoder


# Decoding functions

@docs field
@docs string, int, float, bool, posix, maybe, list
@docs succeed
@docs custom


# Maps and combinators

@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap


# Decoding

@docs Error, decode

-}

import FormToolkit.Form exposing (Form(..))
import FormToolkit.Input as Input exposing (Input)
import FormToolkit.Value as Value
import Internal.Input
import Internal.Tree as Tree exposing (Tree)
import Time


{-| A decoder that takes a tree of input data and returns a decoded result
or an error if the decoding fails.
-}
type Decoder id a
    = Decoder (Tree (Internal.Input.Input id Input.Error) -> Result (Error id) a)


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
field id (Decoder decoder) =
    Decoder
        (Tree.find (Tree.value >> .identifier >> (==) (Just id))
            >> Maybe.map decoder
            >> Maybe.withDefault (Err (InputNotFound id))
        )


{-| TODO
-}
string : Decoder id String
string =
    value (\val -> Value.toString val |> Result.fromMaybe (Input.NotString val))


{-| TODO
-}
int : Decoder id Int
int =
    value (\val -> Value.toInt val |> Result.fromMaybe (Input.NotInt val))


{-| TODO
-}
float : Decoder id Float
float =
    value (\val -> Value.toFloat val |> Result.fromMaybe (Input.NotFloat val))


{-| TODO
-}
bool : Decoder id Bool
bool =
    value (\val -> Value.toBool val |> Result.fromMaybe (Input.NotBool val))


{-| TODO
-}
posix : Decoder id Time.Posix
posix =
    value (\val -> Value.toPosix val |> Result.fromMaybe (Input.NotPosix val))


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
        (Tree.children
            >> List.foldl
                (\e ( prev, i ) ->
                    ( Result.map2 (::)
                        (Result.mapError (ListError i) (decoder e))
                        prev
                    , i + 1
                    )
                )
                ( Ok [], 0 )
            >> Tuple.first
            >> Result.map List.reverse
        )


{-| TODO
-}
succeed : a -> Decoder id a
succeed a =
    Decoder (\_ -> Ok a)


{-| TODO
-}
value : (Value.Value -> Result Input.Error a) -> Decoder id a
value func =
    custom
        (Input.toTree >> Tree.value >> Input.check >> Result.andThen func)


{-| TODO
-}
custom : (Input id -> Result Input.Error a) -> Decoder id a
custom func =
    Decoder
        (\tree ->
            Result.mapError (InputError (.identifier (Tree.value tree)))
                (func (Input.fromTree tree))
        )


{-| TODO
-}
andThen : (a -> Decoder id b) -> Decoder id a -> Decoder id b
andThen callback (Decoder decoder) =
    Decoder
        (\tree ->
            Result.andThen
                (\res ->
                    let
                        (Decoder inner) =
                            callback res
                    in
                    inner tree
                )
                (decoder tree)
        )


{-| TODO
-}
andMap : Decoder id a -> Decoder id (a -> b) -> Decoder id b
andMap (Decoder decoder) (Decoder partial) =
    Decoder
        (\tree ->
            Result.map2 (|>) (decoder tree) (partial tree)
        )


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


{-| Represents an error that occurred during decoding.
-}
type Error id
    = InputError (Maybe id) Input.Error
    | ListError Int (Error id)
    | InputNotFound id


{-| TODO
-}
decode : Decoder id a -> Form id -> Result (Error id) a
decode (Decoder decoder) (Form root) =
    decoder (Input.toTree root)
