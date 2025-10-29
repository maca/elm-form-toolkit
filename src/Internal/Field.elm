module Internal.Field exposing
    ( Field, Attributes, FieldType(..), Status(..)
    , isBlank, isGroup
    , validateNode, validateTree, errors
    , mapAttributes
    )

{-|

@docs Field, Attributes, FieldType, Status
@docs isBlank, isGroup
@docs validateNode, validateTree, errors
@docs mapAttributes

-}

import FormToolkit.Error exposing (Error(..))
import FormToolkit.Value
import Internal.Utils
import Internal.Value as Value exposing (Value)
import List.Extra
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


type alias Attributes id fieldType err =
    { inputType : fieldType
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
    Tree.Tree (Attributes id (FieldType id err) err)


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
            Value.isBlank value


isGroup : Field id err -> Bool
isGroup input =
    case Tree.value input |> .inputType of
        Group ->
            True

        Repeatable _ ->
            True

        _ ->
            False


errors : Field id err -> List err
errors =
    Tree.foldl
        (\node acc ->
            List.concat [ acc, (Tree.value node).errors ]
        )
        []
        >> List.Extra.unique



-- Vaidation


validateNode : Field id (Error id) -> Field id (Error id)
validateNode node =
    let
        ifNotRequired fn =
            if (Tree.value node).isRequired && isBlank node then
                identity

            else
                fn
    in
    List.foldl (<|)
        (clearErrors node)
        [ checkRequired
        , ifNotRequired checkInRange
        , ifNotRequired checkOptionsProvided
        , ifNotRequired checkEmail
        , ifNotRequired checkPattern
        ]


validateTree : Field id (Error id) -> Field id (Error id)
validateTree =
    Tree.map clearErrors >> validateTreeHelp


validateTreeHelp : Field id (Error id) -> Field id (Error id)
validateTreeHelp tree =
    let
        attrs =
            Tree.value (clearErrors tree)
    in
    Tree.branch
        (if attrs.hidden then
            attrs

         else
            Tree.value (validateNode tree)
        )
        (if attrs.hidden then
            Tree.children tree

         else
            Tree.children tree |> List.map validateTree
        )


clearErrors : Field id (Error id) -> Field id (Error id)
clearErrors =
    Tree.updateValue (\attrs -> { attrs | errors = [] })


checkRequired : Field id (Error id) -> Field id (Error id)
checkRequired node =
    if (Tree.value node).isRequired && isBlank node then
        setError IsBlank node

    else
        node


checkInRange : Field id (Error id) -> Field id (Error id)
checkInRange node =
    let
        { value, min, max } =
            Tree.value node

        val =
            FormToolkit.Value.Value value

        minVal =
            FormToolkit.Value.Value min

        maxVal =
            FormToolkit.Value.Value max
    in
    case
        ( Value.compare value min
        , Value.compare value max
        )
    of
        ( Just LT, Just _ ) ->
            setError
                (\id ->
                    ValueNotInRange id
                        { value = val, min = minVal, max = maxVal }
                )
                node

        ( Just _, Just GT ) ->
            setError
                (\id ->
                    ValueNotInRange id
                        { value = val, min = minVal, max = maxVal }
                )
                node

        ( Just LT, Nothing ) ->
            setError
                (\id ->
                    ValueTooSmall id
                        { value = val, min = minVal }
                )
                node

        ( Nothing, Just GT ) ->
            setError
                (\id ->
                    ValueTooLarge id
                        { value = val, max = maxVal }
                )
                node

        _ ->
            node


checkOptionsProvided : Field id (Error id) -> Field id (Error id)
checkOptionsProvided node =
    let
        { inputType, options } =
            Tree.value node
    in
    case ( inputType, options ) of
        ( Select, [] ) ->
            setError NoOptionsProvided node

        ( Radio, [] ) ->
            setError NoOptionsProvided node

        ( StrictAutocomplete, [] ) ->
            setError NoOptionsProvided node

        _ ->
            node


checkEmail : Field id (Error id) -> Field id (Error id)
checkEmail node =
    let
        { value, inputType } =
            Tree.value node
    in
    case inputType of
        Email ->
            case Value.toString value of
                Just str ->
                    if Internal.Utils.isValidEmail str then
                        node

                    else
                        setError EmailInvalid node

                Nothing ->
                    setError ParseError node

        _ ->
            node


checkPattern : Field id (Error id) -> Field id (Error id)
checkPattern node =
    let
        { pattern, value, selectionStart } =
            Tree.value node
    in
    case pattern of
        [] ->
            node

        mask ->
            case Value.toString value of
                Nothing ->
                    setError ParseError node

                Just str ->
                    let
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
                                    | value = Value.fromNonBlankString formatted
                                    , selectionStart = cursorPosition
                                    , selectionEnd = cursorPosition
                                }
                            )
                        |> (if maskConsumed then
                                identity

                            else
                                setError PatternError
                           )


setError : (Maybe id -> Error id) -> Field id (Error id) -> Field id (Error id)
setError errCons =
    Tree.updateValue
        (\attrs ->
            { attrs
                | errors =
                    List.Extra.unique
                        (errCons attrs.identifier :: attrs.errors)
            }
        )


mapAttributes : (a -> b) -> (err1 -> err2) -> Attributes a (FieldType a err1) err1 -> Attributes b (FieldType b err2) err2
mapAttributes func errToErr input =
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
            Repeatable (Tree.mapValues (mapAttributes func errToErr) tree)

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
