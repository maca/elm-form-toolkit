module Internal.Input exposing
    ( Input, InputType(..), Status(..)
    , update, updateWithString
    , resetStatus
    , humanValue
    , init, isBlank, root
    , mapIdentifier
    )

{-|

@docs Input, InputType, Status


# Update

@docs update, updateWithString


# Validation

@docs resetStatus, validate, check


# Value

@docs humanValue


# Errors

@docs init, isBlank, root


# Map

@docs mapIdentifier

-}

-- import FormToolkit.Value as Value

import Array
import Internal.Tree as Tree exposing (Tree)
import Internal.Value exposing (Value)
import List.Extra as List


type Status err
    = Unchecked
    | Valid
    | WithError err


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
    | Element id


type alias Input id err =
    { inputType : InputType id err
    , name : String
    , value : Value
    , isRequired : Bool
    , label : Maybe String
    , hint : Maybe String
    , placeholder : Maybe String
    , min : Value
    , max : Value
    , options : List ( String, Value )
    , inline : Bool
    , identifier : Maybe id
    , status : Status err
    }


root : Input id err
root =
    init Group []


init : InputType id err -> List (Input id err -> Input id err) -> Input id err
init inputType =
    List.foldl (\f i -> f i)
        { inputType = inputType
        , name = ""
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
        , status = Unchecked
        }


update : Value -> Input id err -> Input id err
update value input =
    { input | value = value }


updateWithString : String -> Input id err -> Input id err
updateWithString str ({ inputType } as input) =
    case inputType of
        Text ->
            update (Internal.Value.fromString str) input

        TextArea ->
            update (Internal.Value.fromString str) input

        Password ->
            update (Internal.Value.fromString str) input

        Email ->
            update (Internal.Value.fromString str) input

        Integer ->
            update (Internal.Value.intFromString str) input

        Float ->
            update (Internal.Value.floatFromString str) input

        Month ->
            update (Internal.Value.monthFromString str) input

        Date ->
            update (Internal.Value.dateFromString str) input

        Select ->
            update (getChoice str input) input

        Radio ->
            update (getChoice str input) input

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


resetStatus : Input id err -> Input id err
resetStatus input =
    { input | status = Unchecked }


isBlank : Input id err -> Bool
isBlank { value, inputType } =
    case inputType of
        Group ->
            False

        Repeatable _ ->
            False

        _ ->
            Internal.Value.isBlank value


humanValue : Input id err -> Value
humanValue input =
    case input.inputType of
        Radio ->
            humanValueHelp input

        Select ->
            humanValueHelp input

        _ ->
            input.value


humanValueHelp : Input id err -> Value
humanValueHelp { value, options } =
    List.filter (\( _, v ) -> v == value) options
        |> List.head
        |> Maybe.map (Tuple.first >> Internal.Value.fromString)
        |> Maybe.withDefault Internal.Value.blank


mapIdentifier : (a -> b) -> Input a err -> Input b err
mapIdentifier func input =
    { inputType = mapInputType func input.inputType
    , name = input.name
    , label = input.label
    , hint = input.hint
    , placeholder = input.placeholder
    , status = input.status
    , value = input.value
    , min = input.min
    , max = input.max
    , isRequired = input.isRequired
    , options = input.options
    , inline = input.inline
    , identifier = Maybe.map func input.identifier
    }


mapInputType : (a -> b) -> InputType a err -> InputType b err
mapInputType func inputType =
    case inputType of
        Repeatable tree ->
            Repeatable (Tree.mapValues (mapIdentifier func) tree)

        Element id ->
            Element (func id)

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
