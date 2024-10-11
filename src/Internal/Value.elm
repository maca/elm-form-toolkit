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
    , fromString
    , intFromString
    , isBlank
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
import String.Extra as String
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
    | Blank


toString : Value val -> Result () String
toString value =
    case value of
        Text string ->
            Ok string

        Integer number ->
            Ok (String.fromInt number)

        Float number ->
            Ok (String.fromFloat number)

        Month posix ->
            Ok <| String.slice 0 7 (Iso8601.fromTime posix)

        Date posix ->
            Ok <| String.slice 0 10 (Iso8601.fromTime posix)

        Time posix ->
            Ok (Iso8601.fromTime posix)

        Boolean True ->
            Err ()

        Boolean False ->
            Err ()

        Custom _ ->
            Err ()

        Blank ->
            Err ()


toInt : Value val -> Result () Int
toInt value =
    case value of
        Integer val ->
            Ok val

        _ ->
            Err ()


toFloat : Value val -> Result () Float
toFloat value =
    case value of
        Float val ->
            Ok val

        _ ->
            Err ()


toBool : Value val -> Result () Bool
toBool value =
    case value of
        Boolean val ->
            Ok val

        _ ->
            Err ()


toPosix : Value val -> Result () Posix
toPosix value =
    case value of
        Month val ->
            Ok val

        Date val ->
            Ok val

        Time val ->
            Ok val

        _ ->
            Err ()


toCustom : Value val -> Result () val
toCustom value =
    case value of
        Custom val ->
            Ok val

        _ ->
            Err ()


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
                |> Result.map Encode.string
                |> Result.withDefault Encode.null


fromString : String -> Value val
fromString str =
    String.nonBlank str
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
