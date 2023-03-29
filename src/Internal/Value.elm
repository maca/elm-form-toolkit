module Internal.Value exposing
    ( Value(..)
    , compare
    , dateFromString
    , encode
    , floatFromString
    , intFromString
    , monthFromString
    , timeFromString
    , toHuman
    , toString
    , transformString
    )

import Internal.Time exposing (dateMonthToHuman, dateToHuman)
import Iso8601
import Json.Encode as Encode
import Time exposing (Posix)


type Value
    = Text String
    | Integer Int
    | Float Float
    | Month Posix
    | Date Posix
    | Time Posix
    | Boolean Bool
    | List (List (List ( String, Value )))
    | Blank


toString : Value -> Maybe String
toString value =
    case value of
        Text s ->
            Just s

        Integer n ->
            Just (String.fromInt n)

        Float n ->
            Just (String.fromFloat n)

        Month posix ->
            Just <| String.slice 0 7 (Iso8601.fromTime posix)

        Date posix ->
            Just <| String.slice 0 10 (Iso8601.fromTime posix)

        Time posix ->
            Just (Iso8601.fromTime posix)

        Boolean True ->
            Just "true"

        Boolean False ->
            Just "false"

        _ ->
            Nothing


encode : Value -> Encode.Value
encode value =
    case value of
        Integer n ->
            Encode.int n

        Float n ->
            Encode.float n

        Boolean b ->
            Encode.bool b

        Blank ->
            Encode.null

        _ ->
            toString value
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null


toHuman : Value -> String
toHuman value =
    case value of
        Month posix ->
            dateMonthToHuman posix

        Date posix ->
            dateToHuman posix

        Time posix ->
            Iso8601.fromTime posix

        _ ->
            Maybe.withDefault "?" (toString value)


transformString : (String -> String) -> Value -> Value
transformString f value =
    case value of
        Text val ->
            Text (f val)

        _ ->
            value


intFromString : String -> Value
intFromString str =
    Maybe.map Integer (String.toInt str)
        |> Maybe.withDefault Blank


floatFromString : String -> Value
floatFromString str =
    Maybe.map Float (String.toFloat str)
        |> Maybe.withDefault Blank


monthFromString : String -> Value
monthFromString str =
    Iso8601.toTime (String.slice 0 7 str ++ "-01T00:00")
        |> Result.map Month
        |> Result.toMaybe
        |> Maybe.withDefault Blank


dateFromString : String -> Value
dateFromString str =
    Iso8601.toTime (String.slice 0 10 str ++ "T00:00")
        |> Result.map Date
        |> Result.toMaybe
        |> Maybe.withDefault Blank


timeFromString : String -> Value
timeFromString str =
    Iso8601.toTime str
        |> Result.map Time
        |> Result.toMaybe
        |> Maybe.withDefault Blank


compare : Value -> Value -> Maybe Order
compare a b =
    Maybe.map2 Basics.compare (toNumber a) (toNumber b)


toNumber : Value -> Maybe Float
toNumber value =
    case value of
        Integer n ->
            Just (Basics.toFloat n)

        Float n ->
            Just n

        Month posix ->
            Just <| Basics.toFloat (Time.posixToMillis posix)

        Date posix ->
            Just <| Basics.toFloat (Time.posixToMillis posix)

        Time posix ->
            Just <| Basics.toFloat (Time.posixToMillis posix)

        _ ->
            Nothing
