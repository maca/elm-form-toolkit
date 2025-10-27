module Internal.Field exposing
    ( Field, Attributes, FieldType(..), Status(..)
    , isBlank, isAutocompleteable
    , inputIdString, inputStringToValue
    , touchTree
    , updateValueWithString
    , ParserResult(..), parseFailure, parseSuccess
    , parseString, parseEmail, parseMaskedString
    , parseMap, parseAndThen, parseAndUpdate
    , validate, validateNode, validateNodeParser
    , parseValue, validateTreeParser, combineErrors
    )

{-|

@docs Field, Attributes, FieldType, Status
@docs init, isBlank, map
@docs updateAttributes
@docs isRepeatable, isAutocompleteable
@docs setErrors
@docs inputIdString, inputStringToValue
@docs touchTree
@docs updateValueWithString
@docs ParserResult, parseFailure, parseSuccess
@docs parseString, parseEmail, parseMaskedString
@docs parseMap, parseAndThen, parseAndUpdate
@docs validate, validateNode, validateNodeParser
@docs parseValue, validateTreeParser, combineErrors

-}

import Array
import Dict
import FormToolkit.Error as Error exposing (Error(..))
import FormToolkit.Value as Value
import Internal.Utils
import Internal.Value exposing (Value)
import List.Extra
import Regex
import RoseTree.Tree as Tree


type Status
    = Pristine
    | Focused
    | Touched


type FieldType id err
    = Text
    | TextArea
    | Email
    | Password
    | StrictAutocomplete
    | Integer
    | Float
    | Month
    | Date
    | LocalDatetime
    | Select
    | Radio
    | Checkbox
    | Group
    | Repeatable (Field id err)


type alias Attributes id err =
    { inputType : FieldType id err
    , name : Maybe String
    , value : Value
    , isRequired : Bool
    , label : Maybe String
    , placeholder : Maybe String
    , hint : Maybe String
    , min : Value
    , max : Value
    , step : Value
    , autogrow : Bool
    , options : List ( String, Value )
    , identifier : Maybe id
    , status : Status
    , repeatableMin : Int
    , repeatableMax : Maybe Int
    , addFieldsButtonCopy : String
    , removeFieldsButtonCopy : String
    , errors : List err
    , classList : List String
    , selectionStart : Int
    , selectionEnd : Int
    , disabled : Bool
    , hidden : Bool
    , pattern : List Internal.Utils.MaskToken
    }


type alias Field id err =
    Tree.Tree (Attributes id err)


init : FieldType id err -> List (Attributes id err -> Attributes id err) -> Attributes id err
init inputType_ =
    List.foldl (<|)
        { inputType = inputType_
        , name = Nothing
        , label = Nothing
        , hint = Nothing
        , placeholder = Nothing
        , value = Internal.Value.blank
        , min = Internal.Value.blank
        , max = Internal.Value.blank
        , step = Internal.Value.blank
        , autogrow = False
        , isRequired = False
        , options = []
        , identifier = Nothing
        , status = Pristine
        , repeatableMin = 1
        , repeatableMax = Nothing
        , addFieldsButtonCopy = "Add"
        , removeFieldsButtonCopy = "Remove"
        , errors = []
        , classList = []
        , selectionStart = 0
        , selectionEnd = 0
        , disabled = False
        , hidden = False
        , pattern = []
        }


updateAttributes :
    List (Attributes id err -> Attributes id err)
    -> Field id err
    -> Field id err
updateAttributes attrList =
    Tree.updateValue
        (\attrs ->
            let
                updatedAttrs =
                    List.foldl (<|) attrs attrList
            in
            { updatedAttrs | identifier = attrs.identifier }
        )


touchTree : Field id err -> Field id err
touchTree =
    Tree.mapValues (\node -> { node | status = Touched })


isBlank : Field id err -> Bool
isBlank input =
    let
        { inputType, value } =
            Tree.value input
    in
    case inputType of
        Group ->
            False

        Repeatable _ ->
            False

        _ ->
            Internal.Value.isBlank value


isAutocompleteable : Field id err -> Bool
isAutocompleteable input =
    let
        { inputType, options } =
            Tree.value input
    in
    case inputType of
        Text ->
            not (List.isEmpty options)

        StrictAutocomplete ->
            True

        _ ->
            False


isGroup : Field id err -> Bool
isGroup input =
    case Tree.value input |> .inputType of
        Group ->
            True

        Repeatable _ ->
            True

        _ ->
            False


isRepeatable : Field id err -> Bool
isRepeatable input =
    case Tree.value input |> .inputType of
        Repeatable _ ->
            True

        _ ->
            False


errors : Field id err -> List err
errors =
    Tree.foldl (\node acc -> List.concat [ acc, (Tree.value node).errors ]) []
        >> List.Extra.unique


setErrors : List err -> Field id err -> Field id err
setErrors errorList =
    Tree.updateValue
        (\input ->
            { input
                | errors = List.Extra.unique (errorList ++ input.errors)
            }
        )


clearErrors : Field id err -> Field id err
clearErrors =
    Tree.updateValue (\input -> { input | errors = [] })


map : (a -> b) -> (err1 -> err2) -> Attributes a err1 -> Attributes b err2
map func errToErr input =
    { inputType = mapFieldType func errToErr input.inputType
    , name = input.name
    , value = input.value
    , isRequired = input.isRequired
    , label = input.label
    , placeholder = input.placeholder
    , hint = input.hint
    , min = input.min
    , max = input.max
    , step = input.step
    , autogrow = input.autogrow
    , options = input.options
    , identifier = Maybe.map func input.identifier
    , status = input.status
    , repeatableMin = input.repeatableMin
    , repeatableMax = input.repeatableMax
    , addFieldsButtonCopy = input.addFieldsButtonCopy
    , removeFieldsButtonCopy = input.removeFieldsButtonCopy
    , errors = List.map errToErr input.errors
    , classList = input.classList
    , selectionStart = input.selectionStart
    , selectionEnd = input.selectionEnd
    , disabled = input.disabled
    , hidden = input.hidden
    , pattern = input.pattern
    }


mapFieldType : (a -> b) -> (err1 -> err2) -> FieldType a err1 -> FieldType b err2
mapFieldType func errToErr inputType_ =
    case inputType_ of
        Repeatable tree ->
            Repeatable (Tree.mapValues (map func errToErr) tree)

        Text ->
            Text

        TextArea ->
            TextArea

        Email ->
            Email

        Password ->
            Password

        StrictAutocomplete ->
            StrictAutocomplete

        Integer ->
            Integer

        Float ->
            Float

        Month ->
            Month

        Date ->
            Date

        LocalDatetime ->
            LocalDatetime

        Select ->
            Select

        Radio ->
            Radio

        Checkbox ->
            Checkbox

        Group ->
            Group


inputIdString : Field id err -> String
inputIdString input =
    let
        { name, inputType } =
            Tree.value input
    in
    name
        |> Maybe.withDefault
            (inputTypeToString inputType)


inputTypeToString : FieldType id err -> String
inputTypeToString type_ =
    case type_ of
        Text ->
            "text"

        StrictAutocomplete ->
            "text"

        TextArea ->
            "textarea"

        Email ->
            "email"

        Password ->
            "password"

        Integer ->
            "integer"

        Float ->
            "float"

        Month ->
            "month"

        Date ->
            "date"

        LocalDatetime ->
            "datetime-local"

        Select ->
            "select"

        Radio ->
            "radio"

        Checkbox ->
            "checkbox"

        Repeatable _ ->
            "repeatable"

        Group ->
            "group"


updateValueWithString : String -> Field id err -> Field id err
updateValueWithString str field =
    Tree.updateValue
        (\attrs -> { attrs | value = inputStringToValue field str })
        field



-- Debug.todo "crash"


inputStringToValue : Field id err -> String -> Value
inputStringToValue input str =
    let
        unwrappedField =
            Tree.value input

        getChoice () =
            case String.toInt str of
                Just idx ->
                    Array.fromList unwrappedField.options
                        |> Array.get idx
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault Internal.Value.blank

                Nothing ->
                    Internal.Value.blank
    in
    case unwrappedField.inputType of
        Text ->
            Internal.Value.fromNonBlankString str

        TextArea ->
            Internal.Value.fromNonEmptyString str

        Password ->
            Internal.Value.fromNonBlankString str

        StrictAutocomplete ->
            Dict.fromList unwrappedField.options
                |> Dict.get str
                |> Maybe.withDefault Internal.Value.blank

        Email ->
            Internal.Value.fromNonBlankString str

        Integer ->
            Internal.Value.intFromString str

        Float ->
            Internal.Value.floatFromString str

        Month ->
            Internal.Value.monthFromString str

        Date ->
            Internal.Value.dateFromString str

        LocalDatetime ->
            Internal.Value.timeFromString str

        Select ->
            getChoice ()

        Radio ->
            getChoice ()

        Checkbox ->
            case str of
                "true" ->
                    Internal.Value.fromBool True

                "false" ->
                    Internal.Value.fromBool False

                _ ->
                    Internal.Value.blank

        Group ->
            Internal.Value.blank

        Repeatable _ ->
            Internal.Value.blank



-- Parser functions


type ParserResult id a
    = Failure (Field id (Error id)) (Error id)
    | Success (Field id (Error id)) a


parseFailure : Field id (Error id) -> Error id -> ParserResult id a
parseFailure input err =
    Failure (setErrors [ err ] input) err


parseSuccess : Field id (Error id) -> a -> ParserResult id a
parseSuccess input a =
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


parseAndUpdate :
    (Field id (Error id) -> a -> { field : Field id (Error id), parser : Field id (Error id) -> ParserResult id b })
    -> (Field id (Error id) -> ParserResult id a)
    -> (Field id (Error id) -> ParserResult id b)
parseAndUpdate func parser node =
    case parser node of
        Success input2 a ->
            let
                result =
                    func input2 a

                newParser =
                    result.parser
            in
            newParser result.field

        Failure input2 errorVal ->
            Failure input2 errorVal


parseAndThen : (a -> (Field id (Error id) -> ParserResult id b)) -> (Field id (Error id) -> ParserResult id a) -> (Field id (Error id) -> ParserResult id b)
parseAndThen func parser node =
    case parser node of
        Success input2 a ->
            func a input2

        Failure input2 errorVal ->
            Failure input2 errorVal


parseMap : (a -> b) -> (Field id (Error id) -> ParserResult id a) -> (Field id (Error id) -> ParserResult id b)
parseMap func parser input =
    case parser input of
        Success input2 a ->
            Success input2 (func a)

        Failure input2 errorVal ->
            Failure input2 errorVal


validate : Field id (Error id) -> Field id (Error id)
validate input =
    case validateTreeParser input of
        Success updatedField _ ->
            updatedField

        Failure updatedField _ ->
            updatedField


validateNode : Field id (Error id) -> Field id (Error id)
validateNode input =
    case validateNodeParser input of
        Success updatedField _ ->
            updatedField

        Failure updatedField _ ->
            updatedField


validateTreeParser : Field id (Error id) -> ParserResult id ()
validateTreeParser input =
    let
        updated =
            validateVisible input
    in
    case errors updated of
        [] ->
            Success updated ()

        firstError :: restErrors ->
            Failure updated
                (List.foldl
                    (\err acc -> combineErrors (Tree.value input).identifier acc err)
                    firstError
                    restErrors
                )


validateVisible : Field id (Error id) -> Field id (Error id)
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


validateNodeParser : Field id (Error id) -> ParserResult id ()
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
                ( clearErrors node, Nothing )
                validations
    in
    case maybeError of
        Nothing ->
            Success finalNode ()

        Just error ->
            Failure (setErrors [ error ] finalNode) error


validations : List (Field id (Error id) -> ParserResult id ())
validations =
    [ checkRequired
    , checkInRange
    , checkOptionsProvided
    , checkEmail
    , checkPattern
    ]


checkRequired : Field id (Error id) -> ParserResult id ()
checkRequired node =
    let
        { isRequired, identifier } =
            Tree.value node
    in
    if isRequired && isBlank node then
        parseFailure node (IsBlank identifier)

    else
        parseSuccess node ()


checkInRange : Field id (Error id) -> ParserResult id ()
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
            parseFailure node
                (ValueNotInRange identifier
                    { value = val, min = minVal, max = maxVal }
                )

        ( Just _, Just GT ) ->
            parseFailure node
                (ValueNotInRange identifier
                    { value = val, min = minVal, max = maxVal }
                )

        ( Just LT, Nothing ) ->
            parseFailure node
                (ValueTooSmall identifier
                    { value = val, min = minVal }
                )

        ( Nothing, Just GT ) ->
            parseFailure node
                (ValueTooLarge identifier
                    { value = val, max = maxVal }
                )

        _ ->
            parseSuccess node ()


checkOptionsProvided : Field id (Error id) -> ParserResult id ()
checkOptionsProvided node =
    let
        { inputType, options, identifier } =
            Tree.value node
    in
    case ( inputType, options ) of
        ( Select, [] ) ->
            parseFailure node (NoOptionsProvided identifier)

        ( Radio, [] ) ->
            parseFailure node (NoOptionsProvided identifier)

        ( StrictAutocomplete, [] ) ->
            parseFailure node (NoOptionsProvided identifier)

        _ ->
            parseSuccess node ()


checkEmail : Field id (Error id) -> ParserResult id ()
checkEmail node =
    case (Tree.value node).inputType of
        Email ->
            (parseEmail |> parseMap (always ())) node

        _ ->
            parseSuccess node ()


parseEmail : Field id (Error id) -> ParserResult id String
parseEmail =
    parseString
        |> parseAndThen
            (\str ->
                \input ->
                    if isValidEmail str then
                        parseSuccess input str

                    else
                        parseFailure input (EmailInvalid (Tree.value input).identifier)
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


checkPattern : Field id (Error id) -> ParserResult id ()
checkPattern node =
    case (Tree.value node).pattern of
        [] ->
            Success node ()

        patternTokens ->
            (parseString
                |> parseAndThen (parseMaskedString patternTokens)
                |> parseMap (always ())
            )
                node


{-| -}
parseString : Field id (Error id) -> ParserResult id String
parseString =
    parseValue
        (\id val -> Value.toString val |> Result.fromMaybe (ParseError id))


parseMaskedString : List Internal.Utils.MaskToken -> String -> Field id (Error id) -> ParserResult id String
parseMaskedString mask str node =
    let
        { identifier, selectionStart } =
            Tree.value node

        { formatted, cursorPosition, maskConsumed } =
            Internal.Utils.formatMaskWithTokens
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
                    parseSuccess updatedField formatted

                else
                    parseFailure updatedField (PatternError identifier)
           )


{-| Parse input values using a custom parsing function.
-}
parseValue : (Maybe id -> Value.Value -> Result (Error id) a) -> (Field id (Error id) -> ParserResult id a)
parseValue func node =
    let
        ({ identifier, options, isRequired } as attrs) =
            Tree.value node
    in
    if isGroup node then
        parseFailure node (IsGroupNotInput identifier)

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
            ( isRequired && isBlank node
            , func identifier fieldValue
            , attrs.errors
            )
        of
            ( True, _, _ ) ->
                parseFailure node (IsBlank identifier)

            ( _, Ok a, [] ) ->
                Success node a

            ( _, Ok _, firstError :: restErrors ) ->
                Failure node
                    (List.foldl (\err acc -> combineErrors identifier acc err)
                        firstError
                        restErrors
                    )

            ( _, Err err, [] ) ->
                parseFailure node err

            ( _, Err err, firstError :: restErrors ) ->
                let
                    combinedError =
                        List.foldl
                            (\e acc -> combineErrors identifier acc e)
                            firstError
                            (restErrors ++ [ err ])
                in
                Failure (setErrors [ combinedError ] node) combinedError
