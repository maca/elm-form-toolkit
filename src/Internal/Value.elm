module Internal.Value exposing
    ( Value(..)
    , blank
    , compare
    , dateFromString
    , encode
    , floatFromString
    , fromBool
    , fromFloat
    , fromInt
    , fromNonBlankString
    , fromNonEmptyString
    , intFromString
    , isBlank
    , isInvalid
    , monthFromString
    , toBool
    , toFloat
    , toInt
    , toPosix
    , toString
    )

import Iso8601
import Json.Encode as Encode
import String.Extra
import Time exposing (Posix)


type Value
    = Text String
    | Integer Int
    | Float Float
    | Month Posix
    | Date Posix
    | Time Posix
    | Boolean Bool
    | Invalid
    | Blank


toString : Value -> Maybe String
toString value =
    case value of
        Text string ->
            Just string

        Integer number ->
            Just (String.fromInt number)

        Float number ->
            Just (String.fromFloat number)

        Month posix ->
            Just <| String.slice 0 7 (Iso8601.fromTime posix)

        Date posix ->
            Just <| String.slice 0 10 (Iso8601.fromTime posix)

        Time posix ->
            Just (Iso8601.fromTime posix)

        Boolean True ->
            Nothing

        Boolean False ->
            Nothing

        Invalid ->
            Nothing

        Blank ->
            Nothing


toInt : Value -> Maybe Int
toInt value =
    case value of
        Integer val ->
            Just val

        Text string ->
            String.toInt string

        _ ->
            Nothing


toFloat : Value -> Maybe Float
toFloat value =
    case value of
        Float val ->
            Just val

        Text string ->
            String.toFloat string

        _ ->
            Nothing


toBool : Value -> Maybe Bool
toBool value =
    case value of
        Boolean val ->
            Just val

        _ ->
            Nothing


toPosix : Value -> Maybe Posix
toPosix value =
    case value of
        Month val ->
            Just val

        Date val ->
            Just val

        Time val ->
            Just val

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


fromNonBlankString : String -> Value
fromNonBlankString str =
    String.Extra.nonBlank str
        |> Maybe.map Text
        |> Maybe.withDefault Blank


fromNonEmptyString : String -> Value
fromNonEmptyString str =
    String.Extra.nonEmpty str
        |> Maybe.map Text
        |> Maybe.withDefault Blank


fromInt : Int -> Value
fromInt =
    Integer


fromFloat : Float -> Value
fromFloat =
    Float


fromBool : Bool -> Value
fromBool =
    Boolean


blank : Value
blank =
    Blank


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


isBlank : Value -> Bool
isBlank value =
    case value of
        Blank ->
            True

        _ ->
            False


isInvalid : Value -> Bool
isInvalid value =
    case value of
        Invalid ->
            True

        _ ->
            False
