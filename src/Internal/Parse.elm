module Internal.Parse exposing
    ( ParserResult(..), failure, success
    , string, email, maskedString
    , map, andThen, andUpdate
    , validate, validateNode, validateNodeParser
    , parseValue, validateTreeParser, combineErrors
    )

{-|

@docs ParserResult, failure, success
@docs string, email, maskedString
@docs map, andThen, andUpdate
@docs validate, validateNode, validateNodeParser
@docs parseValue, validateTreeParser, combineErrors

-}

import Dict
import FormToolkit.Error as Error exposing (Error(..))
import FormToolkit.Value as Value
import Internal.Field
import Internal.Utils as Utils
import Internal.Value
import List.Extra
import Regex
import RoseTree.Tree as Tree


type alias Field id =
    Internal.Field.Field id (Error id)


type ParserResult id a
    = Failure (Field id) (Error id)
    | Success (Field id) a


failure : Field id -> Error id -> ParserResult id a
failure input err =
    Failure (Internal.Field.setErrors [ err ] input) err


success : Field id -> a -> ParserResult id a
success input a =
    Success input a


combineErrors : Maybe id -> Error id -> Error id -> Error id
combineErrors identifier err1 err2 =
    if err1 == err2 then
        err1

    else
        ErrorList identifier
            (List.concat [ Error.toList err1, Error.toList err2 ]
                |> List.Extra.unique
            )


andUpdate :
    (Field id -> a -> { field : Field id, parser : Field id -> ParserResult id b })
    -> (Field id -> ParserResult id a)
    -> (Field id -> ParserResult id b)
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


andThen : (a -> (Field id -> ParserResult id b)) -> (Field id -> ParserResult id a) -> (Field id -> ParserResult id b)
andThen func parser node =
    case parser node of
        Success input2 a ->
            func a input2

        Failure input2 errors ->
            Failure input2 errors


map : (a -> b) -> (Field id -> ParserResult id a) -> (Field id -> ParserResult id b)
map func parser input =
    case parser input of
        Success input2 a ->
            Success input2 (func a)

        Failure input2 errors ->
            Failure input2 errors


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


validateTreeParser : Field id -> ParserResult id ()
validateTreeParser input =
    let
        updated =
            validateVisible input
    in
    case Internal.Field.errors updated of
        [] ->
            Success updated ()

        firstError :: restErrors ->
            Failure updated
                (List.foldl
                    (\err acc -> combineErrors (Tree.value input).identifier acc err)
                    firstError
                    restErrors
                )


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


validateNodeParser : Field id -> ParserResult id ()
validateNodeParser node =
    let
        ( finalNode, maybeError ) =
            List.foldl
                (\validation ( currentNode, accError ) ->
                    case validation currentNode of
                        Failure updatedNode validationError ->
                            ( updatedNode
                            , case accError of
                                Nothing ->
                                    Just validationError

                                Just err ->
                                    let
                                        { identifier } =
                                            Tree.value node
                                    in
                                    Just (combineErrors identifier err validationError)
                            )

                        Success updatedNode _ ->
                            ( updatedNode, accError )
                )
                ( Internal.Field.clearErrors node, Nothing )
                validations
    in
    case maybeError of
        Nothing ->
            Success finalNode ()

        Just error ->
            Failure (Internal.Field.setErrors [ error ] finalNode) error


validations : List (Field id -> ParserResult id ())
validations =
    [ checkRequired
    , checkInRange
    , checkOptionsProvided
    , checkEmail
    , checkPattern
    ]


checkRequired : Field id -> ParserResult id ()
checkRequired node =
    let
        { isRequired, identifier } =
            Tree.value node
    in
    if isRequired && Internal.Field.isBlank node then
        failure node (IsBlank identifier)

    else
        success node ()


checkInRange : Field id -> ParserResult id ()
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


checkOptionsProvided : Field id -> ParserResult id ()
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


checkEmail : Field id -> ParserResult id ()
checkEmail node =
    case (Tree.value node).inputType of
        Internal.Field.Email ->
            (email |> map (always ())) node

        _ ->
            success node ()


email : Field id -> ParserResult id String
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


checkPattern : Field id -> ParserResult id ()
checkPattern node =
    case (Tree.value node).pattern of
        [] ->
            Success node ()

        patternTokens ->
            (string
                |> andThen (maskedString patternTokens)
                |> map (always ())
            )
                node


{-| -}
string : Field id -> ParserResult id String
string =
    parseValue
        (\id val -> Value.toString val |> Result.fromMaybe (ParseError id))


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
parseValue : (Maybe id -> Value.Value -> Result (Error id) a) -> (Field id -> ParserResult id a)
parseValue func node =
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

            ( _, Ok _, firstError :: restErrors ) ->
                Failure node
                    (List.foldl (\err acc -> combineErrors identifier acc err)
                        firstError
                        restErrors
                    )

            ( _, Err err, [] ) ->
                failure node err

            ( _, Err err, firstError :: restErrors ) ->
                let
                    combinedError =
                        List.foldl
                            (\e acc -> combineErrors identifier acc e)
                            firstError
                            (restErrors ++ [ err ])
                in
                Failure (Internal.Field.setErrors [ combinedError ] node) combinedError
