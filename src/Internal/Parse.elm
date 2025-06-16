module Internal.Parse exposing
    ( Parser(..)
    , field, string, maybe, list, json, custom, format
    , map, map2, andThen, andMap
    , parseValue, parse, validateAndParse
    )

{-|

@docs Parser
@docs field, string, maybe, list, json, custom, format
@docs map, map2, andThen, andMap
@docs parseValue, parse, validateAndParse

-}

import Dict
import FormToolkit.Error exposing (Error(..))
import FormToolkit.Field exposing (Field(..))
import FormToolkit.Value as Value
import Internal.Field
import Internal.Value
import Json.Decode
import Json.Encode
import List.Extra
import RoseTree.Tree as Tree


type alias InternalField id val =
    Internal.Field.Field id val (Error id val)


type Partial id val a
    = Failure (InternalField id val) (List (Error id val))
    | Success (InternalField id val) a


type Parser id val a
    = Parser (InternalField id val -> Partial id val a)


field : id -> Parser id val a -> Parser id val a
field id parser =
    Parser
        (\tree ->
            case fieldHelp id parser tree of
                ( Just (Success node a), path ) ->
                    Success (Tree.replaceAt path node tree) a

                ( Just (Failure node errors), path ) ->
                    Failure (Tree.replaceAt path node tree) errors

                ( Nothing, _ ) ->
                    failure tree (InputNotFound id)
        )


fieldHelp : id -> Parser id val a -> InternalField id val -> ( Maybe (Partial id val a), List Int )
fieldHelp id parser =
    Tree.foldWithPath
        (\path tree acc ->
            if Internal.Field.identifier tree == Just id then
                ( Just (apply parser tree), path )

            else
                acc
        )
        ( Nothing, [] )


string : Parser id val String
string =
    parseValue Value.toString


maybe : Parser id val a -> Parser id val (Maybe a)
maybe parser =
    Parser
        (\input ->
            if Internal.Field.isBlank input then
                Success input Nothing

            else
                mapHelp Just parser input
        )


list : Parser id val a -> Parser id val (List a)
list parser =
    Parser
        (\input ->
            let
                ( children, result ) =
                    listHelp parser input

                input2 =
                    Tree.branch (Tree.value input) children
            in
            case result of
                Ok elements ->
                    Success input2 elements

                Err errors ->
                    Failure input2 errors
        )


listHelp :
    Parser id val a
    -> InternalField id val
    -> ( List (InternalField id val), Result (List (Error id val)) (List a) )
listHelp parser =
    Tree.children
        >> List.foldr
            (\node ( nodes, result ) ->
                case apply parser node of
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
                                Err (List.Extra.unique (errors2 ++ errors))
                        )
            )
            ( [], Ok [] )


json : Parser id val Json.Decode.Value
json =
    Parser
        (\input ->
            case jsonEncodeObject input of
                Ok a ->
                    Success input a

                Err err ->
                    failure input err
        )


jsonEncodeObject : InternalField id val -> Result (Error id val) Json.Encode.Value
jsonEncodeObject input =
    jsonEncodeHelp input [] |> Result.map Json.Encode.object


jsonEncodeHelp :
    InternalField id val
    -> List ( String, Json.Decode.Value )
    -> Result (Error id val) (List ( String, Json.Decode.Value ))
jsonEncodeHelp input acc =
    let
        accumulate jsonValue =
            case Internal.Field.name input of
                Just name ->
                    Ok (( name, jsonValue ) :: acc)

                Nothing ->
                    Err
                        (RepeatableHasNoName
                            (Internal.Field.identifier input)
                        )
    in
    case Internal.Field.inputType input of
        Internal.Field.Group ->
            case Internal.Field.name input of
                Nothing ->
                    Tree.children input
                        |> List.foldr (\e -> Result.andThen (jsonEncodeHelp e))
                            (Ok acc)

                _ ->
                    Tree.children input
                        |> List.foldr (\e -> Result.andThen (jsonEncodeHelp e))
                            (Ok [])
                        |> Result.map Json.Encode.object
                        |> Result.andThen accumulate

        Internal.Field.Repeatable _ ->
            Tree.children input
                |> List.foldr (\e -> Result.map2 (::) (jsonEncodeObject e)) (Ok [])
                |> Result.map (Json.Encode.list identity)
                |> Result.andThen accumulate

        _ ->
            case Internal.Field.name input of
                Just name ->
                    Ok
                        (( name
                         , Internal.Value.encode (Internal.Field.value input)
                         )
                            :: acc
                        )

                Nothing ->
                    Ok acc


custom : (Value.Value val -> Result String a) -> Parser id val a
custom func =
    parseHelp
        (\input ->
            func (Value.Value (Internal.Field.value input))
                |> Result.mapError
                    (CustomError (Internal.Field.identifier input))
        )


format : (String -> String) -> Parser id val String
format func =
    Parser
        (\input ->
            case
                Internal.Field.value input
                    |> Internal.Value.toString
            of
                Just str ->
                    Success
                        (Tree.updateValue
                            (\attrs ->
                                { attrs | value = Internal.Value.Text (func str) }
                            )
                            input
                        )
                        ()

                Nothing ->
                    Success input ()
        )
        |> andThen (\() -> string)


parseValue : (Value.Value val -> Maybe a) -> Parser id val a
parseValue func =
    parseHelp
        (\input ->
            if Internal.Field.isGroup input then
                Err (IsGroupNotInput (Internal.Field.identifier input))

            else
                Internal.Value.toString (Internal.Field.value input)
                    |> Maybe.andThen
                        (\key ->
                            Internal.Field.options input
                                |> Dict.fromList
                                |> Dict.get key
                        )
                    |> Maybe.withDefault (Internal.Field.value input)
                    |> (func << Value.Value)
                    |> Maybe.map Ok
                    |> Maybe.withDefault
                        (Err (ParseError (Internal.Field.identifier input)))
        )


parseHelp : (InternalField id val -> Result (Error id val) a) -> Parser id val a
parseHelp func =
    Parser
        (\input ->
            case func input of
                Ok a ->
                    Success input a

                Err err ->
                    failure input err
        )


failure : InternalField id val -> Error id val -> Partial id val a
failure input err =
    Failure (Internal.Field.setErrors [ err ] input) [ err ]


andThen : (a -> Parser id val b) -> Parser id val a -> Parser id val b
andThen func (Parser parser) =
    Parser
        (\input ->
            case parser input of
                Success input2 a ->
                    apply (func a) input2

                Failure input2 errors ->
                    Failure input2 errors
        )


andMap : Parser id val a -> Parser id val (a -> b) -> Parser id val b
andMap a b =
    map2 (|>) a b


map : (a -> b) -> Parser id val a -> Parser id val b
map func parser =
    Parser (mapHelp func parser)


mapHelp : (a -> b) -> Parser id val a -> InternalField id val -> Partial id val b
mapHelp func (Parser parser) input =
    case parser input of
        Success input2 a ->
            Success input2 (func a)

        Failure input2 errors ->
            Failure input2 errors


map2 : (a -> b -> c) -> Parser id val a -> Parser id val b -> Parser id val c
map2 func a b =
    Parser
        (\input ->
            case apply a input of
                Success input2 res ->
                    case apply b input2 of
                        Success input3 res2 ->
                            Success input3 (func res res2)

                        Failure input3 errors ->
                            Failure input3 errors

                Failure tree2 errors ->
                    case apply b tree2 of
                        Success input3 _ ->
                            Failure input3 errors

                        Failure input3 errors2 ->
                            Failure input3 (List.Extra.unique (errors2 ++ errors))
        )


{-| Parses an input using the given parser without applying field validations
(required, min, max...).
-}
parse : Parser id val a -> Field id val -> Result (List (Error id val)) a
parse parser (Field input) =
    case apply parser input of
        Success _ a ->
            Ok a

        Failure _ errors ->
            Err errors


{-| Validates and parses an input using the given parser.
-}
validateAndParse :
    Parser id val a
    -> Field id val
    -> ( Field id val, Result (List (Error id val)) a )
validateAndParse parser (Field input) =
    case apply (validateTree |> andThen (\() -> parser)) input of
        Success input2 a ->
            ( Field input2, Ok a )

        Failure input2 errors ->
            ( Field input2, Err errors )


validateTree : Parser id val ()
validateTree =
    Parser
        (\input ->
            let
                validators =
                    [ checkRequired, checkInRange, checkOptionsProvided ]

                updated =
                    Tree.map
                        (\node ->
                            case List.filterMap ((|>) node) validators of
                                [] ->
                                    node

                                errors ->
                                    Internal.Field.setErrors errors node
                        )
                        input
            in
            case Internal.Field.errors updated of
                [] ->
                    Success updated ()

                errors ->
                    Failure updated errors
        )


apply : Parser id val a -> InternalField id val -> Partial id val a
apply (Parser parser) =
    parser


checkRequired : InternalField id val -> Maybe (Error id val)
checkRequired input =
    if
        Internal.Field.isRequired input
            && Internal.Field.isBlank input
    then
        Just (IsBlank (Internal.Field.identifier input))

    else
        Nothing


checkInRange : InternalField id val -> Maybe (Error id val)
checkInRange tree =
    let
        val =
            Value.Value (Internal.Field.value tree)

        min =
            Value.Value (Internal.Field.min tree)

        max =
            Value.Value (Internal.Field.max tree)
    in
    case
        ( Internal.Value.compare
            (Internal.Field.value tree)
            (Internal.Field.min tree)
        , Internal.Value.compare
            (Internal.Field.value tree)
            (Internal.Field.max tree)
        )
    of
        ( Just LT, Just _ ) ->
            Just
                (ValueNotInRange (Internal.Field.identifier tree)
                    { value = val, min = min, max = max }
                )

        ( Just _, Just GT ) ->
            Just
                (ValueNotInRange (Internal.Field.identifier tree)
                    { value = val, min = min, max = max }
                )

        ( Just LT, Nothing ) ->
            Just
                (ValueTooSmall (Internal.Field.identifier tree)
                    { value = val, min = min }
                )

        ( Nothing, Just GT ) ->
            Just
                (ValueTooLarge (Internal.Field.identifier tree)
                    { value = val, max = max }
                )

        _ ->
            Nothing


checkOptionsProvided : InternalField id val -> Maybe (Error id val)
checkOptionsProvided input =
    case
        ( Internal.Field.inputType input
        , Internal.Field.options input
        )
    of
        ( Internal.Field.Select, [] ) ->
            Just (NoOptionsProvided (Internal.Field.identifier input))

        ( Internal.Field.Radio, [] ) ->
            Just (NoOptionsProvided (Internal.Field.identifier input))

        ( Internal.Field.StrictAutocomplete, [] ) ->
            Just (NoOptionsProvided (Internal.Field.identifier input))

        _ ->
            Nothing
