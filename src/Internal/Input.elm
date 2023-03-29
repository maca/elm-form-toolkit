module Internal.Input exposing
    ( Input, InputType(..)
    , update, updateWithString
    , resetStatus, validate, check
    , humanValue
    , error, errorMessage, init, isBlank, root
    )

{-|

@docs Input, Error, InputType


# Update

@docs update, updateWithString


# Validation

@docs resetStatus, validate, check


# Value

@docs humanValue

-}

import Array
import FormToolkit.Error as Error exposing (Error(..))
import FormToolkit.Value as Value
import Internal.Tree exposing (Tree)
import Internal.Value exposing (Value)
import Json.Decode as Decode
import List.Extra as List
import Result exposing (Result)


type Status
    = Unchecked
    | Valid
    | WithError Error


type InputType a
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
    | Repeatable (Tree (Input a))


type alias Input a =
    { inputType : InputType a
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
    , identifier : Maybe a
    }


root : Input a
root =
    init Group []


init : InputType a -> List (Input a -> Input a) -> Input a
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
        , identifier = Nothing
        }


update : Value -> Input a -> Input a
update value input =
    { input
        | value =
            List.foldr (\f v -> f v |> Maybe.withDefault input.value)
                value
                input.parsers
    }


updateWithString : String -> Input a -> Input a
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


getChoice : String -> Input a -> Value
getChoice str { options } =
    case String.toInt str of
        Just idx ->
            Array.fromList options
                |> Array.get idx
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Value.blank

        Nothing ->
            Value.blank


error : Input a -> Maybe Error
error { status } =
    case status of
        WithError err ->
            Just err

        _ ->
            Nothing


validate : Input a -> Input a
validate input =
    case check input of
        Ok _ ->
            { input | status = Valid }

        Err err ->
            { input | status = WithError err }


resetStatus : Input a -> Input a
resetStatus input =
    { input | status = Unchecked }


check : Input a -> Result Error Value
check input =
    checkRequired input
        |> Result.andThen (\_ -> checkInRange input)


checkRequired : Input a -> Result Error Value
checkRequired { isRequired, value } =
    if isRequired && Value.isBlank value then
        Err IsBlank

    else
        Ok value


isBlank : Input a -> Bool
isBlank { value, inputType } =
    case inputType of
        Group ->
            False

        Repeatable _ ->
            False

        _ ->
            Value.isBlank value


checkInRange : Input a -> Result Error Value
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


humanValue : Input a -> Value
humanValue input =
    case input.inputType of
        Radio ->
            humanValueHelp input

        Select ->
            humanValueHelp input

        _ ->
            input.value


humanValueHelp : Input a -> Value
humanValueHelp { value, options } =
    List.filter (\( _, v ) -> v == value) options
        |> List.head
        |> Maybe.map (Tuple.first >> Value.string)
        |> Maybe.withDefault Value.blank
