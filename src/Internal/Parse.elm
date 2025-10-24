module Internal.Parse exposing
    ( Parser
    , ParserResult, failure, success
    , field, list, string, email, json, maybe, formattedString, oneOf
    , map, map2, andThen, andUpdate
    , parse, validate, validateNode
    , parseValue
    )

{-|

@docs Parser
@docs ParserResult, failure, success
@docs field, list, string, email, json, maybe, formattedString, oneOf
@docs map, map2, andThen, andUpdate
@docs parse, validate, validateNode
@docs parseValue

-}

import Dict
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
field id parser tree =
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
maybe parser node =
    if Internal.Field.isBlank node then
        Success node Nothing

    else
        mapHelp Just parser node


list : Parser id a -> Parser id (List a)
list parser node =
    if Tree.value node |> .hidden then
        Success
            (Tree.map (Tree.updateValue (\f -> { f | errors = [] }))
                node
            )
            []

    else
        let
            ( children, result ) =
                listHelp parser node

            input2 =
                Tree.branch (Tree.value node) children
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
oneOf parsers node =
    oneOfHelp parsers node []


oneOfHelp : List (Parser id a) -> Field id -> List (Error id) -> ParserResult id a
oneOfHelp parsers input accErrors =
    case parsers of
        [] ->
            case accErrors of
                [] ->
                    failure input (ParseError (Tree.value input |> .identifier))

                errors ->
                    failure input
                        (OneOf (Tree.value input |> .identifier)
                            (List.reverse errors)
                        )

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
andUpdate func parser node =
    case parser node of
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
andThen func parser node =
    case parser node of
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
map2 func a b node =
    case a node of
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
    case map2 (\a _ -> a) parser validateNodeParser input of
        Success input2 a ->
            ( input2, Ok a )

        Failure input2 errors ->
            ( input2, Err errors )


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
checkRequired node =
    let
        { isRequired, identifier } =
            Tree.value node
    in
    if isRequired && Internal.Field.isBlank node then
        failure node (IsBlank identifier)

    else
        success node ()


checkInRange : Parser id ()
checkInRange node =
    let
        { value, min, max, identifier } =
            Tree.value node

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
            failure node
                (ValueNotInRange identifier
                    { value = val, min = minVal, max = maxVal }
                )

        ( Just _, Just GT ) ->
            failure node
                (ValueNotInRange identifier
                    { value = val, min = minVal, max = maxVal }
                )

        ( Just LT, Nothing ) ->
            failure node
                (ValueTooSmall identifier
                    { value = val, min = minVal }
                )

        ( Nothing, Just GT ) ->
            failure node
                (ValueTooLarge identifier
                    { value = val, max = maxVal }
                )

        _ ->
            success node ()


checkOptionsProvided : Parser id ()
checkOptionsProvided node =
    let
        { inputType, options, identifier } =
            Tree.value node
    in
    case ( inputType, options ) of
        ( Internal.Field.Select, [] ) ->
            failure node (NoOptionsProvided identifier)

        ( Internal.Field.Radio, [] ) ->
            failure node (NoOptionsProvided identifier)

        ( Internal.Field.StrictAutocomplete, [] ) ->
            failure node (NoOptionsProvided identifier)

        _ ->
            success node ()


checkEmail : Parser id ()
checkEmail node =
    case (Tree.value node).inputType of
        Internal.Field.Email ->
            (email |> map (always ())) node

        _ ->
            success node ()


email : Parser id String
email =
    string
        |> andThen
            (\str ->
                \input ->
                    if isValidEmail str then
                        success input str

                    else
                        failure input (EmailInvalid (Tree.value input).identifier)
            )


isValidEmail : String -> Bool
isValidEmail =
    let
        -- Standard HTML5 email validation pattern used by browsers
        pattern =
            "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)+$"

        regex =
            Maybe.withDefault Regex.never (Regex.fromString pattern)
    in
    Regex.contains regex


checkPattern : Parser id ()
checkPattern input =
    case (Tree.value input).pattern of
        [] ->
            Success input ()

        patternTokens ->
            (string
                |> andThen (maskedString patternTokens)
                |> map (always ())
            )
                input


{-| Format a string parser with a mask pattern, updating the field's display value and cursor position.
-}
formattedString : String -> Parser id String
formattedString mask =
    string |> andThen (maskedString (Utils.parseMask mask))


{-| -}
string : Parser id String
string =
    parseValue
        (\id val ->
            Value.toString val
                |> Result.fromMaybe (ParseError id)
        )


maskedString : List Utils.MaskToken -> String -> Field id -> ParserResult id String
maskedString mask str node =
    let
        { identifier, selectionStart } =
            Tree.value node

        { formatted, cursorPosition, maskConsumed } =
            Utils.formatMaskWithTokens
                { mask = mask
                , input = str
                , cursorPosition = selectionStart
                }
    in
    node
        |> Tree.updateValue
            (\attrs ->
                { attrs
                    | value = Internal.Value.fromNonBlankString formatted
                    , selectionStart = cursorPosition
                    , selectionEnd = cursorPosition
                }
            )
        |> (\updatedField ->
                if maskConsumed then
                    success updatedField formatted

                else
                    failure updatedField (PatternError identifier)
           )


{-| Parse input values using a custom parsing function.
-}
parseValue : (Maybe id -> Value.Value -> Result (Error id) a) -> Parser id a
parseValue func =
    \node ->
        let
            ({ identifier, options, errors, isRequired } as attrs) =
                Tree.value node
        in
        if Internal.Field.isGroup node then
            failure node (IsGroupNotInput identifier)

        else
            let
                fieldValue =
                    Internal.Value.toString attrs.value
                        |> Maybe.andThen
                            (\key ->
                                options
                                    |> Dict.fromList
                                    |> Dict.get key
                            )
                        |> Maybe.withDefault attrs.value
                        |> Value.Value
            in
            case
                ( isRequired && Internal.Field.isBlank node
                , func identifier fieldValue
                , errors
                )
            of
                ( True, _, _ ) ->
                    failure node (IsBlank identifier)

                ( _, Ok a, [] ) ->
                    Success node a

                ( _, Ok _, errs ) ->
                    Failure node errs

                ( _, Err err, errs ) ->
                    Failure (Internal.Field.setErrors [ err ] node) (err :: errs)
