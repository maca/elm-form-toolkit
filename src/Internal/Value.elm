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
    , mapCustom
    , monthFromString
    , toBool
    , toCustom
    , toFloat
    , toInt
    , toPosix
    , toString
    )

import Iso8601
import Json.Encode as Encode
import String.Extra
import Time exposing (Posix)


type Value val
    = Text String
    | Integer Int
    | Float Float
    | Month Posix
    | Date Posix
    | Time Posix
    | Boolean Bool
    | Custom val
    | Invalid
    | Blank


toString : Value val -> Maybe String
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

        Custom _ ->
            Nothing

        Invalid ->
            Nothing

        Blank ->
            Nothing


toInt : Value val -> Maybe Int
toInt value =
    case value of
        Integer val ->
            Just val

        _ ->
            Nothing


toFloat : Value val -> Maybe Float
toFloat value =
    case value of
        Float val ->
            Just val

        _ ->
            Nothing


toBool : Value val -> Maybe Bool
toBool value =
    case value of
        Boolean val ->
            Just val

        _ ->
            Nothing


toPosix : Value val -> Maybe Posix
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


toCustom : Value val -> Maybe val
toCustom value =
    case value of
        Custom val ->
            Just val

        _ ->
            Nothing


encode : Value val -> Encode.Value
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


fromNonBlankString : String -> Value val
fromNonBlankString str =
    String.Extra.nonBlank str
        |> Maybe.map Text
        |> Maybe.withDefault Blank


fromNonEmptyString : String -> Value val
fromNonEmptyString str =
    String.Extra.nonEmpty str
        |> Maybe.map Text
        |> Maybe.withDefault Blank


fromInt : Int -> Value val
fromInt =
    Integer


fromFloat : Float -> Value val
fromFloat =
    Float


fromBool : Bool -> Value val
fromBool =
    Boolean


blank : Value val
blank =
    Blank


intFromString : String -> Value val
intFromString str =
    Maybe.map Integer (String.toInt str)
        |> Maybe.withDefault Blank


floatFromString : String -> Value val
floatFromString str =
    Maybe.map Float (String.toFloat str)
        |> Maybe.withDefault Blank


monthFromString : String -> Value val
monthFromString str =
    Iso8601.toTime (String.slice 0 7 str ++ "-01T00:00")
        |> Result.map Month
        |> Result.toMaybe
        |> Maybe.withDefault Blank


dateFromString : String -> Value val
dateFromString str =
    Iso8601.toTime (String.slice 0 10 str ++ "T00:00")
        |> Result.map Date
        |> Result.toMaybe
        |> Maybe.withDefault Blank


compare : Value val -> Value val -> Maybe Order
compare a b =
    Maybe.map2 Basics.compare (toNumber a) (toNumber b)


toNumber : Value val -> Maybe Float
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


isBlank : Value val -> Bool
isBlank value =
    case value of
        Blank ->
            True

        _ ->
            False


isInvalid : Value val -> Bool
isInvalid value =
    case value of
        Invalid ->
            True

        _ ->
            False


mapCustom : (val1 -> val2) -> Value val1 -> Value val2
mapCustom func value =
    case value of
        Text string ->
            Text string

        Integer int ->
            Integer int

        Float float ->
            Float float

        Month posix ->
            Month posix

        Date posix ->
            Date posix

        Time posix ->
            Time posix

        Boolean bool ->
            Boolean bool

        Custom val ->
            Custom (func val)

        Invalid ->
            Invalid

        Blank ->
            Blank
