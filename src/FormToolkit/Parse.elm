module FormToolkit.Parse exposing
    ( Parser
    , field
    , string, int, float, bool, posix, maybe
    , custom, value
    , succeed
    , map, map2, map3, map4, map5, map6, map7, map8
    , andThen, andMap
    , parse
    )

{-|

@docs Parser

@docs field
@docs string, int, float, bool, posix, maybe
@docs custom, value

@docs succeed
@docs map, map2, map3, map4, map5, map6, map7, map8
@docs andThen, andMap

@docs parse

-}

import FormToolkit.Form exposing (Form(..))
import FormToolkit.Input as Input
import FormToolkit.Value as Value exposing (Value)
import Internal.Input as Input exposing (Input)
import Internal.Tree as Tree exposing (Tree)
import Time


type Error id
    = InputError (Maybe id) Input.Error
    | InputNotFound id


type alias Parser id a =
    Tree (Input id) -> Result (Error id) a


field : id -> Parser id a -> Parser id a
field id parser =
    Tree.find (Tree.value >> .identifier >> (==) (Just id))
        >> Maybe.map parser
        >> Maybe.withDefault (Err (InputNotFound id))


string : Parser id String
string tree =
    value Value.toString tree


int : Parser id Int
int tree =
    value Value.toInt tree


float : Parser id Float
float tree =
    value Value.toFloat tree


bool : Parser id Bool
bool tree =
    value Value.toBool tree


posix : Parser id Time.Posix
posix tree =
    value Value.toPosix tree


maybe : Parser id a -> Parser id (Maybe a)
maybe parser tree =
    if Input.isBlank (Tree.value tree) then
        succeed Nothing tree

    else
        Result.map Just (parser tree)


value : (Value -> Result Input.Error a) -> Parser id a
value func =
    custom (Tree.value >> Input.check >> Result.andThen func)


custom : (Tree (Input id) -> Result Input.Error a) -> Parser id a
custom func tree =
    Result.mapError (InputError (.identifier (Tree.value tree))) (func tree)


succeed : a -> Parser id a
succeed a =
    always (Ok a)


andThen : (a -> Parser id b) -> Parser id a -> Parser id b
andThen callback parser tree =
    Result.andThen (\res -> callback res tree) (parser tree)


andMap : Parser id a -> Parser id (a -> b) -> Parser id b
andMap parser partial tree =
    Result.map2 (|>) (parser tree) (partial tree)


map : (a -> b) -> Parser id a -> Parser id b
map func parser tree =
    Result.map func (parser tree)


map2 : (a -> b -> c) -> Parser id a -> Parser id b -> Parser id c
map2 func a b =
    map func a |> andMap b


map3 :
    (a -> b -> c -> out)
    -> Parser id a
    -> Parser id b
    -> Parser id c
    -> Parser id out
map3 func a b c =
    map2 func a b |> andMap c


map4 :
    (a -> b -> c -> d -> out)
    -> Parser id a
    -> Parser id b
    -> Parser id c
    -> Parser id d
    -> Parser id out
map4 func a b c d =
    map3 func a b c |> andMap d


map5 :
    (a -> b -> c -> d -> e -> out)
    -> Parser id a
    -> Parser id b
    -> Parser id c
    -> Parser id d
    -> Parser id e
    -> Parser id out
map5 func a b c d e =
    map4 func a b c d |> andMap e


map6 :
    (a -> b -> c -> d -> e -> f -> out)
    -> Parser id a
    -> Parser id b
    -> Parser id c
    -> Parser id d
    -> Parser id e
    -> Parser id f
    -> Parser id out
map6 func a b c d e f =
    map5 func a b c d e |> andMap f


map7 :
    (a -> b -> c -> d -> e -> f -> g -> out)
    -> Parser id a
    -> Parser id b
    -> Parser id c
    -> Parser id d
    -> Parser id e
    -> Parser id f
    -> Parser id g
    -> Parser id out
map7 func a b c d e f g =
    map6 func a b c d e f |> andMap g


map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> out)
    -> Parser id a
    -> Parser id b
    -> Parser id c
    -> Parser id d
    -> Parser id e
    -> Parser id f
    -> Parser id g
    -> Parser id h
    -> Parser id out
map8 func a b c d e f g h =
    map7 func a b c d e f g |> andMap h


parse : Parser id a -> Form id -> Result (Error id) a
parse parser (Form root) =
    parser root
