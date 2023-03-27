module Internal.Input exposing
    ( Input, Error(..), InputType(..)
    , update, updateWithString
    , resetStatus, validate, check
    , humanValue
    , error, errorMessage, init, isBlank, root
    )

{-|

@docs Input, Error, InputType, Attribute


# Update

@docs update, updateWithString


# Validation

@docs resetStatus, validate, check


# Value

@docs humanValue

-}

import Array
import FormToolkit.Value as Value exposing (Value)
import Internal.Tree exposing (Tree)
import Json.Decode as Decode
import List.Extra as List
import Result exposing (Result)


type Status
    = Unchecked
    | Valid
    | WithError Error


type InputType
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
    | Repeatable (Tree Input)


type Error
    = TooLarge Value
    | TooSmall Value
    | NotInRange ( Value, Value )
    | NotInOptions
    | IsBlank
    | DecodeError Decode.Error


type alias Input =
    { inputType : InputType
    , name : String
    , value : Value
    , isRequired : Bool
    , label : Maybe String
    , hint : Maybe String
    , placeholder : Maybe String
    , help : Maybe String
    , min : Value
    , max : Value
    , options : List ( String, Value )
    , parsers : List (Value -> Maybe Value)
    , status : Status
    , inline : Bool
    }


root : Input
root =
    init Group []


init : InputType -> List (Input -> Input) -> Input
init inputType =
    List.foldl (\f i -> f i)
        { inputType = inputType
        , name = ""
        , label = Nothing
        , help = Nothing
        , hint = Nothing
        , placeholder = Nothing
        , status = Unchecked
        , parsers = []
        , value = Value.blank
        , min = Value.blank
        , max = Value.blank
        , isRequired = False
        , options = []
        , inline = False
        }


update : Value -> Input -> Input
update value input =
    { input
        | value =
            List.foldr (\f v -> f v |> Maybe.withDefault input.value)
                value
                input.parsers
    }


updateWithString : String -> Input -> Input
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
            update (Value.intFromString str) input

        Float ->
            update (Value.floatFromString str) input

        Month ->
            update (Value.monthFromString str) input

        Date ->
            update (Value.dateFromString str) input

        Select ->
            update (getChoice str input) input

        Radio ->
            update (getChoice str input) input

        _ ->
            input


getChoice : String -> Input -> Value
getChoice str { options } =
    case String.toInt str of
        Just idx ->
            Array.fromList options
                |> Array.get idx
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Value.blank

        Nothing ->
            Value.blank


error : Input -> Maybe Error
error { status } =
    case status of
        WithError err ->
            Just err

        _ ->
            Nothing


validate : Input -> Input
validate input =
    case check input of
        Ok () ->
            { input | status = Valid }

        Err err ->
            { input | status = WithError err }


resetStatus : Input -> Input
resetStatus input =
    { input | status = Unchecked }


check : Input -> Result Error ()
check input =
    checkRequired input
        |> Result.andThen (\() -> checkInRange input)


checkRequired : Input -> Result Error ()
checkRequired { isRequired, value } =
    if isRequired && Value.isBlank value then
        Err IsBlank

    else
        Ok ()


isBlank : Input -> Bool
isBlank { value, inputType } =
    case inputType of
        Group ->
            False

        Repeatable _ ->
            False

        _ ->
            Value.isBlank value


checkInRange : Input -> Result Error ()
checkInRange { value, min, max } =
    case ( Value.compare value min, Value.compare value max ) of
        ( Just LT, Just _ ) ->
            Err (NotInRange ( min, max ))

        ( Just _, Just GT ) ->
            Err (NotInRange ( min, max ))

        ( Just LT, Nothing ) ->
            Err (TooSmall min)

        ( Nothing, Just GT ) ->
            Err (TooLarge max)

        _ ->
            Ok ()


errorMessage : Status -> Maybe String
errorMessage status =
    let
        value =
            Value.toString >> Maybe.withDefault ""
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


humanValue : Input -> Value
humanValue input =
    case input.inputType of
        Radio ->
            humanValueHelp input

        Select ->
            humanValueHelp input

        _ ->
            input.value


humanValueHelp : Input -> Value
humanValueHelp { value, options } =
    List.filter (\( _, v ) -> v == value) options
        |> List.head
        |> Maybe.map (Tuple.first >> Value.string)
        |> Maybe.withDefault Value.blank
