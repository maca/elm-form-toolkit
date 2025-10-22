module Internal.Parse exposing
    ( Parser
    , ParserResult, failure, success
    , field, list, json, maybe, formattedString, oneOf
    , map, map2, andThen, andUpdate
    , parse, validate
    )

{-|

@docs Parser
@docs ParserResult, failure, success
@docs field, list, json, maybe, formattedString, oneOf
@docs map, map2, andThen, andUpdate
@docs parse, validate

-}

import FormToolkit.Error exposing (Error(..))
import FormToolkit.Value as Value
import Internal.Field
import Internal.Utils as Utils
import Internal.Value
import Json.Decode
import Json.Encode
import List.Extra
import Regex
import RoseTree.Tree as Tree


type alias Field id =
    Internal.Field.Field id (Error id)


type ParserResult id a
    = Failure (Field id) (List (Error id))
    | Success (Field id) a


failure : Field id -> Error id -> ParserResult id a
failure input err =
    Failure (Internal.Field.setErrors [ err ] input) [ err ]


success : Field id -> a -> ParserResult id a
success input a =
    Success input a


type alias Parser id a =
    Field id -> ParserResult id a


field : id -> Parser id a -> Parser id a
field id parser =
    \tree ->
        case fieldHelp id (map2 (\_ a -> a) validateTree parser) tree of
            ( Just (Success node a), path ) ->
                Success (Tree.replaceAt path node tree) a

            ( Just (Failure node errors), path ) ->
                Failure (Tree.replaceAt path node tree) errors

            ( Nothing, _ ) ->
                failure tree (InputNotFound id)


fieldHelp : id -> Parser id a -> Field id -> ( Maybe (ParserResult id a), List Int )
fieldHelp id parser =
    Tree.foldWithPath
        (\path tree acc ->
            if Internal.Field.identifier tree == Just id then
                ( Just (parser tree), path )

            else
                acc
        )
        ( Nothing, [] )


maybe : Parser id a -> Parser id (Maybe a)
maybe parser =
    \input ->
        if Internal.Field.isBlank input then
            Success input Nothing

        else
            mapHelp Just parser input


list : Parser id a -> Parser id (List a)
list parser =
    \input ->
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


listHelp : Parser id a -> Field id -> ( List (Field id), Result (List (Error id)) (List a) )
listHelp parser =
    Tree.children
        >> List.foldr
            (\node ( nodes, result ) ->
                case parser node of
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


oneOf : List (Parser id a) -> Parser id a
oneOf parsers =
    \input ->
        oneOfHelp parsers input []


oneOfHelp : List (Parser id a) -> Field id -> List (Error id) -> ParserResult id a
oneOfHelp parsers input accErrors =
    case parsers of
        [] ->
            case accErrors of
                [] ->
                    failure input (ParseError (Internal.Field.identifier input))
                
                errors ->
                    failure input (OneOf (Internal.Field.identifier input) (List.reverse errors))

        parser :: rest ->
            case parser input of
                Success input2 a ->
                    Success input2 a

                Failure _ newErrors ->
                    oneOfHelp rest input (newErrors ++ accErrors)


json : Parser id Json.Decode.Value
json =
    map2 (always identity)
        validateTree
        (\input ->
            case jsonEncodeObject input of
                Ok a ->
                    Success input a

                Err err ->
                    failure input err
        )


jsonEncodeObject : Field id -> Result (Error id) Json.Encode.Value
jsonEncodeObject input =
    jsonEncodeHelp input [] |> Result.map Json.Encode.object


jsonEncodeHelp :
    Field id
    -> List ( String, Json.Decode.Value )
    -> Result (Error id) (List ( String, Json.Decode.Value ))
jsonEncodeHelp input acc =
    let
        accumulate jsonValue =
            case Internal.Field.name input of
                Just name ->
                    Ok (( name, jsonValue ) :: acc)

                Nothing ->
                    Err
                        (HasNoName (Internal.Field.identifier input))
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


andUpdate :
    (Field id -> a -> { field : Field id, parser : Parser id b })
    -> Parser id a
    -> Parser id b
andUpdate func parser =
    \input ->
        case parser input of
            Success input2 a ->
                let
                    result =
                        func input2 a

                    newParser =
                        result.parser
                in
                newParser result.field

            Failure input2 errors ->
                Failure input2 errors


andThen : (a -> Parser id b) -> Parser id a -> Parser id b
andThen func parser =
    \input ->
        case parser input of
            Success input2 a ->
                func a input2

            Failure input2 errors ->
                Failure input2 errors


map : (a -> b) -> Parser id a -> Parser id b
map func parser =
    mapHelp func parser


mapHelp : (a -> b) -> Parser id a -> Field id -> ParserResult id b
mapHelp func parser input =
    case parser input of
        Success input2 a ->
            Success input2 (func a)

        Failure input2 errors ->
            Failure input2 errors


map2 : (a -> b -> c) -> Parser id a -> Parser id b -> Parser id c
map2 func a b =
    \tree ->
        case a tree of
            Success tree2 res ->
                case b tree2 of
                    Success tree3 res2 ->
                        Success tree3 (func res res2)

                    Failure tree3 errors ->
                        Failure tree3 errors

            Failure tree2 errors ->
                case b tree2 of
                    Success tree3 _ ->
                        Failure tree3 errors

                    Failure tree3 errors2 ->
                        Failure tree3 (List.Extra.unique (errors2 ++ errors))


parse : Parser id a -> Field id -> ( Field id, Result (List (Error id)) a )
parse parser input =
    case map2 (\_ a -> a) validateNode parser input of
        Success input2 a ->
            ( input2, Ok a )

        Failure input2 errors ->
            ( input2, Err errors )


validate : Field id -> Field id
validate input =
    case validateTree input of
        Success updatedField _ ->
            updatedField

        Failure updatedField _ ->
            updatedField


validateTree : Parser id ()
validateTree input =
    let
        updated =
            Tree.map
                (\node ->
                    case validateNode node of
                        Success updatedField _ ->
                            updatedField

                        Failure updatedField _ ->
                            updatedField
                )
                input
    in
    case Internal.Field.errors updated of
        [] ->
            Success updated ()

        errors ->
            Failure updated errors


validateNode : Parser id ()
validateNode node =
    let
        ( finalNode, allErrors ) =
            List.foldl
                (\validation ( currentNode, errors ) ->
                    case validation currentNode of
                        Failure _ validationErrors ->
                            ( currentNode, errors ++ validationErrors )

                        Success updatedNode _ ->
                            ( updatedNode, errors )
                )
                ( node, [] )
                validations
    in
    case allErrors of
        [] ->
            Success finalNode ()

        errors ->
            Failure (Internal.Field.setErrors errors finalNode) errors


validations : List (Parser id ())
validations =
    [ checkRequired
    , checkInRange
    , checkOptionsProvided
    , checkEmail
    , checkPattern
    ]


checkRequired : Parser id ()
checkRequired input =
    if
        Internal.Field.isRequired input
            && Internal.Field.isBlank input
    then
        failure input (IsBlank (Internal.Field.identifier input))

    else
        success input ()


checkInRange : Parser id ()
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
            failure tree
                (ValueNotInRange (Internal.Field.identifier tree)
                    { value = val, min = min, max = max }
                )

        ( Just _, Just GT ) ->
            failure tree
                (ValueNotInRange (Internal.Field.identifier tree)
                    { value = val, min = min, max = max }
                )

        ( Just LT, Nothing ) ->
            failure tree
                (ValueTooSmall (Internal.Field.identifier tree)
                    { value = val, min = min }
                )

        ( Nothing, Just GT ) ->
            failure tree
                (ValueTooLarge (Internal.Field.identifier tree)
                    { value = val, max = max }
                )

        _ ->
            success tree ()


checkOptionsProvided : Parser id ()
checkOptionsProvided input =
    case
        ( Internal.Field.inputType input
        , Internal.Field.options input
        )
    of
        ( Internal.Field.Select, [] ) ->
            failure input (NoOptionsProvided (Internal.Field.identifier input))

        ( Internal.Field.Radio, [] ) ->
            failure input (NoOptionsProvided (Internal.Field.identifier input))

        ( Internal.Field.StrictAutocomplete, [] ) ->
            failure input (NoOptionsProvided (Internal.Field.identifier input))

        _ ->
            success input ()


checkEmail : Parser id ()
checkEmail input =
    case Internal.Field.inputType input of
        Internal.Field.Email ->
            Internal.Value.toString (Internal.Field.value input)
                |> Maybe.andThen
                    (\value ->
                        if isValidEmail value then
                            Nothing

                        else
                            Just (EmailInvalid (Internal.Field.identifier input))
                    )
                |> Maybe.map (failure input)
                |> Maybe.withDefault (success input ())

        _ ->
            success input ()


isValidEmail : String -> Bool
isValidEmail email =
    let
        -- Standard HTML5 email validation pattern used by browsers
        pattern =
            "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)+$"

        regex =
            Maybe.withDefault Regex.never (Regex.fromString pattern)
    in
    Regex.contains regex email


checkPattern : Parser id ()
checkPattern input =
    let
        patternTokens =
            Internal.Field.pattern input
    in
    if List.isEmpty patternTokens then
        success input ()

    else
        case Internal.Value.toString (Internal.Field.value input) of
            Just rawInput ->
                let
                    fieldAttributes =
                        Tree.value input

                    { formatted, cursorPosition, maskConsumed } =
                        Utils.formatMaskWithTokens
                            { mask = patternTokens
                            , input = rawInput
                            , cursorPosition = fieldAttributes.selectionStart
                            }
                in
                if maskConsumed then
                    let
                        updatedField =
                            Tree.updateValue
                                (\attrs ->
                                    { attrs
                                        | value = Internal.Value.fromNonBlankString formatted
                                        , selectionStart = cursorPosition
                                        , selectionEnd = cursorPosition
                                    }
                                )
                                input
                    in
                    success updatedField ()

                else
                    failure input (PatternError (Internal.Field.identifier input))

            Nothing ->
                if Internal.Field.isBlank input then
                    success input ()

                else
                    failure input (PatternError (Internal.Field.identifier input))


{-| Format a string parser with a mask pattern, updating the field's display value and cursor position.
-}
formattedString : String -> Parser id String
formattedString mask =
    \input ->
        case Internal.Value.toString (Internal.Field.value input) of
            Just rawInput ->
                let
                    fieldAttributes =
                        Tree.value input

                    { formatted, cursorPosition, maskConsumed } =
                        Utils.formatMask
                            { mask = mask
                            , input = rawInput
                            , cursorPosition = fieldAttributes.selectionStart
                            }

                    updatedField =
                        Tree.updateValue
                            (\attrs ->
                                { attrs
                                    | value = Internal.Value.fromNonBlankString formatted
                                    , selectionStart = cursorPosition
                                    , selectionEnd = cursorPosition
                                }
                            )
                            input
                in
                if maskConsumed then
                    success updatedField formatted

                else
                    failure updatedField (PatternError (Internal.Field.identifier input))

            Nothing ->
                if Internal.Field.isRequired input && Internal.Field.isBlank input then
                    failure input (IsBlank (Internal.Field.identifier input))

                else
                    failure input (ParseError (Internal.Field.identifier input))
