module Internal.Input exposing
    ( Input, InputType(..)
    , update, updateWithString
    , resetStatus, validate, check
    , humanValue
    , error, errorMessage, init, isBlank, root
    , mapIdentifier
    )

{-|

@docs Input, InputType


# Update

@docs update, updateWithString


# Validation

@docs resetStatus, validate, check


# Value

@docs humanValue


# Errors

@docs error, errorMessage, init, isBlank, root


# Map

@docs mapIdentifier

-}

import Array
import FormToolkit.Error exposing (Error(..))
import FormToolkit.Value as Value
import Internal.Tree as Tree exposing (Tree)
import Internal.Value exposing (Value)
import List.Extra as List
import Result exposing (Result)


type Status
    = Unchecked
    | Valid
    | WithError Error


type InputType id
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
    | Repeatable (Tree (Input id))


type alias Input id =
    { inputType : InputType id
    , name : String
    , value : Value
    , isRequired : Bool
    , label : Maybe String
    , hint : Maybe String
    , placeholder : Maybe String
    , min : Value
    , max : Value
    , options : List ( String, Value )
    , status : Status
    , inline : Bool
    , identifier : Maybe id
    }


root : Input id
root =
    init Group []


init : InputType id -> List (Input id -> Input id) -> Input id
init inputType =
    List.foldl (\f i -> f i)
        { inputType = inputType
        , name = ""
        , label = Nothing
        , hint = Nothing
        , placeholder = Nothing
        , status = Unchecked
        , value = Value.blank
        , min = Value.blank
        , max = Value.blank
        , isRequired = False
        , options = []
        , inline = False
        , identifier = Nothing
        }


update : Value -> Input id -> Input id
update value input =
    { input | value = value }


updateWithString : String -> Input id -> Input id
updateWithString str ({ inputType } as input) =
    case inputType of
        Text ->
            update (Value.string str) input

        TextArea ->
            update (Value.string str) input

        Password ->
            update (Value.string str) input

        Email ->
            update (Value.string str) input

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


getChoice : String -> Input id -> Value
getChoice str { options } =
    case String.toInt str of
        Just idx ->
            Array.fromList options
                |> Array.get idx
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Value.blank

        Nothing ->
            Value.blank


error : Input id -> Maybe Error
error { status } =
    case status of
        WithError err ->
            Just err

        _ ->
            Nothing


validate : Input id -> Input id
validate input =
    case check input of
        Ok _ ->
            { input | status = Valid }

        Err err ->
            { input | status = WithError err }


resetStatus : Input id -> Input id
resetStatus input =
    { input | status = Unchecked }


check : Input id -> Result Error Value
check input =
    checkRequired input
        |> Result.andThen (\_ -> checkInRange input)


checkRequired : Input id -> Result Error Value
checkRequired { isRequired, value } =
    if isRequired && Value.isBlank value then
        Err IsBlank

    else
        Ok value


isBlank : Input id -> Bool
isBlank { value, inputType } =
    case inputType of
        Group ->
            False

        Repeatable _ ->
            False

        _ ->
            Value.isBlank value


checkInRange : Input id -> Result Error Value
checkInRange { value, min, max } =
    case
        ( Internal.Value.compare value min
        , Internal.Value.compare value max
        )
    of
        ( Just LT, Just _ ) ->
            Err (NotInRange ( min, max ))

        ( Just _, Just GT ) ->
            Err (NotInRange ( min, max ))

        ( Just LT, Nothing ) ->
            Err (TooSmall min)

        ( Nothing, Just GT ) ->
            Err (TooLarge max)

        _ ->
            Ok value


errorMessage : Status -> Maybe String
errorMessage status =
    let
        value =
            Internal.Value.toString >> Maybe.withDefault ""
    in
    case status of
        WithError (TooLarge max) ->
            Just ("Should be lesser than " ++ value max)

        WithError (TooSmall min) ->
            Just ("Should be greater than " ++ value min)

        WithError (NotInRange ( min, max )) ->
            Just ("Should be between " ++ value min ++ " and " ++ value max)

        WithError NotInOptions ->
            Just "Is not one of the allowed options"

        WithError IsBlank ->
            Just "Please fill in this input"

        _ ->
            Nothing


humanValue : Input id -> Value
humanValue input =
    case input.inputType of
        Radio ->
            humanValueHelp input

        Select ->
            humanValueHelp input

        _ ->
            input.value


humanValueHelp : Input id -> Value
humanValueHelp { value, options } =
    List.filter (\( _, v ) -> v == value) options
        |> List.head
        |> Maybe.map (Tuple.first >> Value.string)
        |> Maybe.withDefault Value.blank


mapIdentifier : (a -> b) -> Input a -> Input b
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


mapInputType : (a -> b) -> InputType a -> InputType b
mapInputType func inputType =
    case inputType of
        Repeatable tree ->
            Repeatable (Tree.mapValues (mapIdentifier func) tree)

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
