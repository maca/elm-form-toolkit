module Internal.Input exposing
    ( Input, InputType(..), Status(..)
    , blur, focus, init, isBlank, map, root
    , updateValue, updateValueWithString
    )

{-|

@docs Input, InputType, Status
@docs blur, focus, init, isBlank, map, root, updateWithString
@docs updateValue, updateValueWithString
@docs Msg, update

-}

import Array
import Internal.Value exposing (Value)
import List.Extra as List
import RoseTree.Tree as Tree exposing (Tree)


type Status
    = Pristine
    | Focused
    | Touched


type InputType id err
    = Text
    | TextArea
    | Password
    | Email
    | Integer
    | Float
    | Month
    | Date
    | Select
    | Radio
    | Checkbox
    | Group
    | Repeatable (Tree (Input id err))


type alias Input id err =
    { inputType : InputType id err
    , name : Maybe String
    , value : Value
    , isRequired : Bool
    , label : Maybe String
    , placeholder : Maybe String
    , hint : Maybe String
    , min : Value
    , max : Value
    , options : List ( String, Value )
    , inline : Bool
    , identifier : Maybe id
    , status : Status
    , repeatableMin : Int
    , repeatableMax : Maybe Int
    , addInputsText : String
    , removeInputsText : String
    , errors : List err
    }


root : Input id err
root =
    init Group []


init : InputType id err -> List (Input id err -> Input id err) -> Input id err
init inputType =
    List.foldl (\f i -> f i)
        { inputType = inputType
        , name = Nothing
        , label = Nothing
        , hint = Nothing
        , placeholder = Nothing
        , value = Internal.Value.blank
        , min = Internal.Value.blank
        , max = Internal.Value.blank
        , isRequired = False
        , options = []
        , inline = False
        , identifier = Nothing
        , status = Pristine
        , repeatableMin = 1
        , repeatableMax = Nothing
        , addInputsText = "Add"
        , removeInputsText = "Remove"
        , errors = []
        }


updateValue : Value -> Input id err -> Input id err
updateValue value input =
    { input | value = value, errors = [] }


updateValueWithString : String -> Input id err -> Input id err
updateValueWithString str ({ inputType } as input) =
    case inputType of
        Text ->
            updateValue (Internal.Value.fromString str) input

        TextArea ->
            updateValue (Internal.Value.fromString str) input

        Password ->
            updateValue (Internal.Value.fromString str) input

        Email ->
            updateValue (Internal.Value.fromString str) input

        Integer ->
            updateValue (Internal.Value.intFromString str) input

        Float ->
            updateValue (Internal.Value.floatFromString str) input

        Month ->
            updateValue (Internal.Value.monthFromString str) input

        Date ->
            updateValue (Internal.Value.dateFromString str) input

        Select ->
            updateValue (getChoice str input) input

        Radio ->
            updateValue (getChoice str input) input

        _ ->
            input


getChoice : String -> Input id err -> Value
getChoice str { options } =
    case String.toInt str of
        Just idx ->
            Array.fromList options
                |> Array.get idx
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Internal.Value.blank

        Nothing ->
            Internal.Value.blank


focus : Input id err -> Input id err
focus input =
    { input | status = Focused }


blur : Input id err -> Input id err
blur input =
    { input | status = Touched }


isBlank : Input id err -> Bool
isBlank { value, inputType } =
    case inputType of
        Group ->
            False

        Repeatable _ ->
            False

        _ ->
            Internal.Value.isBlank value


map : (a -> b) -> (err1 -> err2) -> Input a err1 -> Input b err2
map func errToErr input =
    { inputType = mapInputType func errToErr input.inputType
    , name = input.name
    , value = input.value
    , isRequired = input.isRequired
    , label = input.label
    , placeholder = input.placeholder
    , hint = input.hint
    , min = input.min
    , max = input.max
    , options = input.options
    , inline = input.inline
    , identifier = Maybe.map func input.identifier
    , status = input.status
    , repeatableMin = input.repeatableMin
    , repeatableMax = input.repeatableMax
    , addInputsText = input.addInputsText
    , removeInputsText = input.removeInputsText
    , errors = List.map errToErr input.errors
    }


mapInputType : (a -> b) -> (err1 -> err2) -> InputType a err1 -> InputType b err2
mapInputType func errToErr inputType =
    case inputType of
        Repeatable tree ->
            Repeatable (Tree.mapValues (map func errToErr) tree)

        Text ->
            Text

        TextArea ->
            TextArea

        Password ->
            Password

        Email ->
            Email

        Integer ->
            Integer

        Float ->
            Float

        Month ->
            Month

        Date ->
            Date

        Select ->
            Select

        Radio ->
            Radio

        Checkbox ->
            Checkbox

        Group ->
            Group
