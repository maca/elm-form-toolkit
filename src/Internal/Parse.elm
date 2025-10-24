module Internal.Parse exposing
    ( Parser
    , ParserResult, failure, success
    , field, list, json, maybe, formattedString, oneOf
    , map, map2, andThen, andUpdate
    , parse, validate, validateNode
    )

{-|

@docs Parser
@docs ParserResult, failure, success
@docs field, list, json, maybe, formattedString, oneOf
@docs map, map2, andThen, andUpdate
@docs parse, validate, validateNode

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
        case fieldHelp id (map2 (\_ a -> a) validateTreeParser parser) tree of
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
            if (Tree.value tree |> .identifier) == Just id then
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
        if Tree.value input |> .hidden then
            Success
                (Tree.map (Tree.updateValue (\f -> { f | errors = [] }))
                    input
                )
                []

        else
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
                    failure input (ParseError (Tree.value input |> .identifier))

                errors ->
                    failure input (OneOf (Tree.value input |> .identifier) (List.reverse errors))

        parser :: rest ->
            case parser input of
                Success input2 a ->
                    Success input2 a

                Failure _ newErrors ->
                    oneOfHelp rest input (newErrors ++ accErrors)


json : Parser id Json.Decode.Value
json =
    map2 (always identity)
        validateTreeParser
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
        { name, inputType, identifier, value } =
            Tree.value input

        accumulate jsonValue =
            case name of
                Just n ->
                    Ok (( n, jsonValue ) :: acc)

                Nothing ->
                    Err
                        (HasNoName identifier)
    in
    case inputType of
        Internal.Field.Group ->
            case name of
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
            case name of
                Just n ->
                    Ok
                        (( n
                         , Internal.Value.encode value
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
    case parser input of
        Success input2 a ->
            case validateNodeParser input2 of
                Success input3 _ ->
                    ( input3, Ok a )

                Failure input3 errors ->
                    ( input3, Err errors )

        Failure input2 errors ->
            case validateNodeParser input2 of
                Success input3 _ ->
                    ( input3, Err errors )

                Failure input3 errors2 ->
                    ( input3, Err (errors2 ++ errors) )


validate : Field id -> Field id
validate input =
    case validateTreeParser input of
        Success updatedField _ ->
            updatedField

        Failure updatedField _ ->
            updatedField


validateNode : Field id -> Field id
validateNode input =
    case validateNodeParser input of
        Success updatedField _ ->
            updatedField

        Failure updatedField _ ->
            updatedField


validateTreeParser : Parser id ()
validateTreeParser input =
    let
        updated =
            validateVisible input
    in
    case Internal.Field.errors updated of
        [] ->
            Success updated ()

        errors ->
            Failure updated errors


validateVisible : Field id -> Field id
validateVisible tree =
    let
        { hidden } =
            Tree.value tree

        validatedNode =
            if hidden then
                Tree.value tree

            else
                case validateNodeParser tree of
                    Success updatedField _ ->
                        Tree.value updatedField

                    Failure updatedField _ ->
                        Tree.value updatedField
    in
    Tree.branch validatedNode
        (if hidden then
            Tree.children tree

         else
            Tree.children tree |> List.map validateVisible
        )


validateNodeParser : Parser id ()
validateNodeParser node =
    let
        ( finalNode, allErrors ) =
            List.foldl
                (\validation ( currentNode, errors ) ->
                    case validation currentNode of
                        Failure updatedNode validationErrors ->
                            ( updatedNode, errors ++ validationErrors )

                        Success updatedNode _ ->
                            ( updatedNode, errors )
                )
                ( Internal.Field.clearErrors node, [] )
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
    let
        { isRequired, identifier } =
            Tree.value input
    in
    if
        isRequired
            && Internal.Field.isBlank input
    then
        failure input (IsBlank identifier)

    else
        success input ()


checkInRange : Parser id ()
checkInRange tree =
    let
        { value, min, max, identifier } =
            Tree.value tree

        val =
            Value.Value value

        minVal =
            Value.Value min

        maxVal =
            Value.Value max
    in
    case
        ( Internal.Value.compare value min
        , Internal.Value.compare value max
        )
    of
        ( Just LT, Just _ ) ->
            failure tree
                (ValueNotInRange identifier
                    { value = val, min = minVal, max = maxVal }
                )

        ( Just _, Just GT ) ->
            failure tree
                (ValueNotInRange identifier
                    { value = val, min = minVal, max = maxVal }
                )

        ( Just LT, Nothing ) ->
            failure tree
                (ValueTooSmall identifier
                    { value = val, min = minVal }
                )

        ( Nothing, Just GT ) ->
            failure tree
                (ValueTooLarge identifier
                    { value = val, max = maxVal }
                )

        _ ->
            success tree ()


checkOptionsProvided : Parser id ()
checkOptionsProvided input =
    let
        { inputType, options, identifier } =
            Tree.value input
    in
    case ( inputType, options ) of
        ( Internal.Field.Select, [] ) ->
            failure input (NoOptionsProvided identifier)

        ( Internal.Field.Radio, [] ) ->
            failure input (NoOptionsProvided identifier)

        ( Internal.Field.StrictAutocomplete, [] ) ->
            failure input (NoOptionsProvided identifier)

        _ ->
            success input ()


checkEmail : Parser id ()
checkEmail input =
    let
        { inputType, value, identifier } =
            Tree.value input
    in
    case inputType of
        Internal.Field.Email ->
            Internal.Value.toString value
                |> Maybe.andThen
                    (\val ->
                        if isValidEmail val then
                            Nothing

                        else
                            Just (EmailInvalid identifier)
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
    case (Tree.value input).pattern of
        [] ->
            Success input ()

        patternTokens ->
            case formattedStringHelp patternTokens input of
                Success input2 _ ->
                    Success input2 ()

                Failure input2 errors ->
                    Failure input2 errors


{-| Format a string parser with a mask pattern, updating the field's display value and cursor position.
-}
formattedString : String -> Parser id String
formattedString mask =
    formattedStringHelp (Utils.parseMask mask)


formattedStringHelp : List Utils.MaskToken -> Parser id String
formattedStringHelp mask =
    \input ->
        let
            { value, identifier, isRequired, selectionStart } =
                Tree.value input
        in
        case Internal.Value.toString value of
            Just rawInput ->
                let
                    { formatted, cursorPosition, maskConsumed } =
                        Utils.formatMaskWithTokens
                            { mask = mask
                            , input = rawInput
                            , cursorPosition = selectionStart
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
                    failure updatedField (PatternError identifier)

            Nothing ->
                if isRequired && Internal.Field.isBlank input then
                    failure input (IsBlank identifier)

                else
                    failure input (ParseError identifier)
