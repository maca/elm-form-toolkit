module TreeTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Tree as Tree exposing (Tree)
import Test exposing (..)


suite : Test
suite =
    describe "a Tree"
        [ test "makeTree helper" <|
            \_ ->
                makeTree 2 3
                    |> Expect.equal
                        (Tree.branch 0
                            [ Tree.branch 1
                                [ Tree.branch 3
                                    [ Tree.leaf 7, Tree.leaf 8 ]
                                , Tree.branch 4
                                    [ Tree.leaf 9, Tree.leaf 10 ]
                                ]
                            , Tree.branch 2
                                [ Tree.branch 5
                                    [ Tree.leaf 11, Tree.leaf 12 ]
                                , Tree.branch 6
                                    [ Tree.leaf 13, Tree.leaf 14 ]
                                ]
                            ]
                        )
        , describe "value"
            [ test "creates leaf with value" <|
                \_ ->
                    Tree.leaf 1
                        |> Tree.value
                        |> Expect.equal 1
            , test "creates branch with value" <|
                \_ ->
                    Tree.branch 1 []
                        |> Tree.value
                        |> Expect.equal 1
            , test "sets value" <|
                \_ ->
                    Tree.branch 1 []
                        |> Tree.setValue 2
                        |> Tree.value
                        |> Expect.equal 2
            , test "updates value" <|
                \_ ->
                    Tree.branch 2 []
                        |> Tree.updateValue ((*) 2)
                        |> Tree.value
                        |> Expect.equal 4
            ]
        , describe "get"
            [ test "value at path" <|
                \_ ->
                    makeTree 2 3
                        |> Expect.all
                            [ Tree.get [ 1, 1, 1 ]
                                >> Expect.equal (Just (Tree.leaf 14))
                            , Tree.get [ 0, 0, 0 ]
                                >> Expect.equal (Just (Tree.leaf 7))
                            , Tree.get [ 0, 1, 0 ]
                                >> Expect.equal (Just (Tree.leaf 9))
                            , Tree.get [ 0, 1, 1 ]
                                >> Expect.equal (Just (Tree.leaf 10))
                            ]
            , fuzz (searchFuzzer 5 5) "exists at path" <|
                \{ breadth, depth, path } ->
                    makeTree breadth depth
                        |> Tree.get path
                        |> Expect.notEqual Nothing
            , fuzz (searchFuzzer 5 5) "doesn't exist at search too deep" <|
                \{ breadth, depth } ->
                    makeTree breadth depth
                        |> Tree.get (List.repeat (depth + 1) 0)
                        |> Expect.equal Nothing
            , fuzz (searchFuzzer 5 5) "doesn't exist at search too wide" <|
                \{ breadth, depth } ->
                    makeTree breadth depth
                        |> Tree.get (List.repeat depth (breadth + 1))
                        |> Expect.equal Nothing
            ]
        , describe "update"
            [ fuzz (searchFuzzer 5 5) "at path" <|
                \{ breadth, depth, path } ->
                    makeTree breadth depth
                        |> Tree.update path (Tree.setValue -1)
                        |> Tree.get path
                        |> Maybe.map Tree.value
                        |> Expect.equal (Just -1)
            , fuzz (searchFuzzer 2 5) "not affecting if out of bounds" <|
                \{ breadth, depth, path } ->
                    makeTree breadth depth
                        |> Tree.update (List.map (\_ -> breadth + 1) path)
                            (Tree.setValue -1)
                        |> Expect.equal (makeTree breadth depth)
            ]
        , describe "set"
            [ fuzz (searchFuzzer 5 5) "at path" <|
                \{ breadth, depth, path } ->
                    makeTree breadth depth
                        |> Tree.set path replacement
                        |> Tree.get path
                        |> Expect.equal (Just replacement)
            , fuzz (searchFuzzer 2 5) "not affecting if out of bounds" <|
                \{ breadth, depth, path } ->
                    makeTree breadth depth
                        |> Tree.set (List.map (\_ -> breadth + 1) path)
                            replacement
                        |> Expect.equal (makeTree breadth depth)
            ]
        , describe "remove"
            [ fuzz (searchFuzzer 3 3) "last child" <|
                \{ breadth, depth, path } ->
                    makeTree breadth depth
                        |> Tree.remove (pathForLast breadth path)
                        |> Tree.get (pathForLast breadth path)
                        |> Expect.equal Nothing
            , fuzz (searchFuzzer 3 3) "first child" <|
                \{ breadth, depth, path } ->
                    makeTree breadth depth
                        |> Tree.remove (pathForFirst path)
                        |> Tree.get (pathForFirst path)
                        |> Expect.equal
                            (makeTree breadth depth
                                |> Tree.get (replaceLastIdx 1 path)
                            )
            , fuzz (searchFuzzer 2 5) "not affecting if out of bounds" <|
                \{ breadth, depth, path } ->
                    makeTree breadth depth
                        |> Tree.remove (List.map (\_ -> breadth + 1) path)
                        |> Expect.equal (makeTree breadth depth)
            ]
        , describe "insert before"
            [ fuzz (searchFuzzer 3 3) "last child" <|
                \({ breadth, depth } as search) ->
                    let
                        tree =
                            makeTree breadth depth

                        path =
                            pathForLast breadth search.path
                    in
                    tree
                        |> Tree.insertBefore path replacement
                        |> Expect.all
                            [ Tree.get path
                                >> Expect.equal (Just replacement)
                            , Tree.get (pathForLast (breadth + 1) path)
                                >> Expect.notEqual Nothing
                            , Tree.get (pathForLast (breadth + 1) path)
                                >> Expect.equal (Tree.get path tree)
                            ]
            , fuzz (searchFuzzer 3 3) "first child" <|
                \({ breadth, depth } as search) ->
                    let
                        tree =
                            makeTree breadth depth

                        path =
                            pathForFirst search.path
                    in
                    tree
                        |> Tree.insertBefore path replacement
                        |> Expect.all
                            [ Tree.get path
                                >> Expect.equal (Just replacement)
                            , Tree.get (replaceLastIdx 1 path)
                                >> Expect.notEqual Nothing
                            , Tree.get (replaceLastIdx 1 path)
                                >> Expect.equal (Tree.get path tree)
                            ]
            , fuzz (searchFuzzer 2 5) "not affecting if out of bounds" <|
                \{ breadth, depth, path } ->
                    makeTree breadth depth
                        |> Tree.insertBefore (List.map (\_ -> breadth + 1) path)
                            replacement
                        |> Expect.equal (makeTree breadth depth)
            ]
        , describe "insert after"
            [ fuzz (searchFuzzer 3 3) "last child" <|
                \({ breadth, depth } as search) ->
                    let
                        tree =
                            makeTree breadth depth

                        path =
                            pathForLast breadth search.path
                    in
                    tree
                        |> Tree.insertAfter path replacement
                        |> Expect.all
                            [ Tree.get path
                                >> Expect.equal (Tree.get path tree)
                            , Tree.get (pathForLast (breadth + 1) path)
                                >> Expect.notEqual Nothing
                            , Tree.get (pathForLast (breadth + 1) path)
                                >> Expect.equal (Just replacement)
                            ]
            , fuzz (searchFuzzer 3 3) "first child" <|
                \({ breadth, depth } as search) ->
                    let
                        tree =
                            makeTree breadth depth

                        path =
                            pathForFirst search.path
                    in
                    tree
                        |> Tree.insertAfter path replacement
                        |> Expect.all
                            [ Tree.get path
                                >> Expect.equal (Tree.get path tree)
                            , Tree.get (replaceLastIdx 1 path)
                                >> Expect.notEqual Nothing
                            , Tree.get (replaceLastIdx 1 path)
                                >> Expect.equal (Just replacement)
                            ]
            , fuzz (searchFuzzer 2 5) "not affecting if out of bounds" <|
                \{ breadth, depth, path } ->
                    makeTree breadth depth
                        |> Tree.insertAfter (List.map (\_ -> breadth + 1) path)
                            replacement
                        |> Expect.equal (makeTree breadth depth)
            ]
        , describe "map"
            [ fuzz (searchFuzzer 5 5) "applies function to all nodes" <|
                \{ breadth, depth, path } ->
                    let
                        expected =
                            (makeTree breadth depth
                                |> Tree.foldl (Tree.value >> (+)) 0
                            )
                                * 2
                    in
                    makeTree breadth depth
                        |> Tree.map (Tree.updateValue ((*) 2))
                        |> Tree.foldl (Tree.value >> (+)) 0
                        |> Expect.equal expected
            ]
        , describe "mapValues"
            [ fuzz (searchFuzzer 5 5) "applies function to all values" <|
                \{ breadth, depth, path } ->
                    let
                        expected =
                            (makeTree breadth depth
                                |> Tree.foldl (Tree.value >> (+)) 0
                            )
                                * 2
                    in
                    makeTree breadth depth
                        |> Tree.mapValues ((*) 2)
                        |> Tree.foldl (Tree.value >> (+)) 0
                        |> Expect.equal expected
            ]
        , describe "any"
            [ fuzz (searchFuzzer 3 3) "there is one" <|
                \({ breadth, depth } as search) ->
                    let
                        tree =
                            makeTree breadth depth
                                |> Tree.insertAfter path replacement

                        path =
                            pathForLast breadth search.path
                    in
                    Expect.equal True (Tree.any ((==) replacement) tree)
            , fuzz (searchFuzzer 3 3) "there is none" <|
                \search ->
                    let
                        tree =
                            makeTree search.breadth search.depth
                    in
                    Expect.equal False (Tree.any ((==) replacement) tree)
            ]
        ]



-- HELPERS


replacement : Tree Int
replacement =
    Tree.leaf -1


pathForLast : Int -> List Int -> List Int
pathForLast breadth path =
    replaceLastIdx (breadth - 1) path


pathForFirst : List Int -> List Int
pathForFirst path =
    replaceLastIdx 0 path


replaceLastIdx : Int -> List Int -> List Int
replaceLastIdx idx path =
    case List.reverse path of
        _ :: p ->
            List.reverse (idx :: p)

        [] ->
            []


makeTree : Int -> Int -> Tree Int
makeTree breadth depth =
    Tree.branch 0 (makeChildren 1 breadth (depth - 1))


makeChildren : Int -> Int -> Int -> List (Tree Int)
makeChildren id breadth depth =
    List.range id (breadth + id - 1)
        |> List.map
            (\i ->
                if depth <= 0 then
                    Tree.leaf i

                else
                    Tree.branch i
                        (makeChildren ((breadth * i) + (breadth - 1))
                            breadth
                            (depth - 1)
                        )
            )



-- FUZZERS


type alias Search =
    { breadth : Int
    , depth : Int
    , path : List Int
    }


searchFuzzer : Int -> Int -> Fuzzer Search
searchFuzzer maxBreadth maxDepth =
    Fuzz.pair (Fuzz.intRange 1 maxBreadth) (Fuzz.intRange 1 maxDepth)
        |> Fuzz.andThen
            (\( breadth, depth ) ->
                Fuzz.map (\pathDepth -> ( breadth, depth, pathDepth ))
                    (Fuzz.intRange 1 depth)
            )
        |> Fuzz.andThen
            (\( breadth, depth, pathDepth ) ->
                Fuzz.map (Search breadth depth)
                    (Fuzz.listOfLength pathDepth
                        (Fuzz.intRange 0 (breadth - 1))
                    )
            )
