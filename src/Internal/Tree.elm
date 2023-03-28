module Internal.Tree exposing
    ( Tree
    , branch, leaf
    , value, setValue, updateValue
    , children, unshift, push
    , get, getValue, update, set, remove, insertBefore, insertAfter
    , map, mapValues, filterMap, foldl, foldr, find, any
    , last
    )

{-|

@docs Tree


# Create

@docs branch, leaf


# Properties

@docs value, setValue, updateValue


# Children

@docs children, unshift, push


# Traversing

@docs get, getValue, update, set, remove, insertBefore, insertAfter


# Folds

@docs map, mapValues, filterMap, foldl, foldr, find, any

-}

import Array exposing (Array)
import Array.Extra as Array


type Tree a
    = Tree a (Array (Tree a))


leaf : a -> Tree a
leaf a =
    Tree a Array.empty


branch : a -> List (Tree a) -> Tree a
branch a ns =
    Tree a (Array.fromList ns)


value : Tree a -> a
value (Tree a _) =
    a


setValue : a -> Tree a -> Tree a
setValue a (Tree _ ns) =
    Tree a ns


updateValue : (a -> a) -> Tree a -> Tree a
updateValue f (Tree a ns) =
    Tree (f a) ns


children : Tree a -> List (Tree a)
children (Tree _ ns) =
    Array.toList ns


unshift : Tree a -> Tree a -> Tree a
unshift n (Tree a ns) =
    Tree a (Array.append (Array.fromList [ n ]) ns)


push : Tree a -> Tree a -> Tree a
push n (Tree a ns) =
    Tree a (Array.push n ns)


get : List Int -> Tree a -> Maybe (Tree a)
get path (Tree _ ns) =
    case path of
        idx :: [] ->
            Array.get idx ns

        idx :: rest ->
            Maybe.andThen (get rest) (Array.get idx ns)

        [] ->
            Nothing


getValue : List Int -> Tree a -> Maybe a
getValue path tree =
    get path tree |> Maybe.map value


update : List Int -> (Tree a -> Tree a) -> Tree a -> Tree a
update path f node =
    updateHelp path (\idx -> Array.update idx f) node


set : List Int -> Tree a -> Tree a -> Tree a
set path inserted node =
    update path (always inserted) node


remove : List Int -> Tree a -> Tree a
remove path node =
    updateHelp path Array.removeAt node


insertBefore : List Int -> Tree a -> Tree a -> Tree a
insertBefore path inserted =
    updateHelp path (insertHelp identity inserted)


insertAfter : List Int -> Tree a -> Tree a -> Tree a
insertAfter path inserted =
    updateHelp path (insertHelp ((+) 1) inserted)


map : (Tree a -> Tree a) -> Tree a -> Tree a
map f (Tree a ns) =
    f (Tree a (Array.map (map f) ns))


mapValues : (a -> b) -> Tree a -> Tree b
mapValues f (Tree a ns) =
    Tree (f a) (Array.map (mapValues f) ns)


filterMap : (Tree a -> Maybe (Tree a)) -> Tree a -> Tree a
filterMap f (Tree a ns) =
    Tree a (Array.filterMap (filterMap f >> f) ns)


foldl : (Tree a -> b -> b) -> b -> Tree a -> b
foldl f acc (Tree a ns) =
    Array.foldl (\n acc_ -> foldl f acc_ n) (f (Tree a ns) acc) ns


foldr : (Tree a -> b -> b) -> b -> Tree a -> b
foldr f acc (Tree a ns) =
    Array.foldr (\n acc_ -> foldr f acc_ n) (f (Tree a ns) acc) ns


any : (Tree a -> Bool) -> Tree a -> Bool
any pred =
    foldl (\n acc -> acc || pred n) False


{-| TODO: tests
-}
find : (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
find pred =
    foldl
        (\n acc ->
            if pred n && acc == Nothing then
                Just n

            else
                acc
        )
        Nothing


last : Tree a -> Maybe (Tree a)
last node =
    let
        lastNode =
            lastHelp node
    in
    if lastNode == node then
        Nothing

    else
        Just lastNode



-- HELPERS


lastHelp : Tree a -> Tree a
lastHelp node =
    case List.reverse (children node) of
        x :: _ ->
            lastHelp x

        _ ->
            node


updateHelp :
    List Int
    -> (Int -> Array (Tree a) -> Array (Tree a))
    -> Tree a
    -> Tree a
updateHelp path f (Tree a ns) =
    case path of
        idx :: [] ->
            Tree a (f idx ns)

        idx :: rest ->
            Tree a (Array.update idx (updateHelp rest f) ns)

        [] ->
            Tree a ns


insertHelp : (Int -> Int) -> Tree a -> Int -> Array (Tree a) -> Array (Tree a)
insertHelp f inserted idx ns =
    if Array.length ns < idx then
        ns

    else
        let
            ( h, t ) =
                Array.splitAt (f idx) ns
        in
        Array.append (Array.push inserted h) t
