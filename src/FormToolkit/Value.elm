module FormToolkit.Value exposing
    ( Value
    , string, int, float, date, month, time, boolean, list, blank
    , intFromString, floatFromString
    , monthFromString, dateFromString, timeFromString
    , toString, toInt, toFloat, toBool, toList, toHuman
    , isBlank, compare
    , encode, transformString
    )

{-|

@docs Value


# Create

@docs string, int, float, date, month, time, boolean, list, blank


# From String

@docs intFromString, floatFromString
@docs monthFromString, dateFromString, timeFromString


# Convert

@docs toString, toInt, toFloat, toBool, toList, toHuman


# Other

@docs isBlank, compare


# Other

encode

-}

import Internal.Time exposing (dateMonthToHuman, dateToHuman)
import Iso8601
import Json.Encode as Encode
import String.Extra as String
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


string : String -> Value
string str =
    String.nonBlank str
        |> Maybe.map Text
        |> Maybe.withDefault Blank


int : Int -> Value
int =
    Integer


intFromString : String -> Value
intFromString str =
    Maybe.map int (String.toInt str)
        |> Maybe.withDefault Blank


float : Float -> Value
float =
    Float


floatFromString : String -> Value
floatFromString str =
    Maybe.map float (String.toFloat str)
        |> Maybe.withDefault Blank


month : Posix -> Value
month =
    Month


monthFromString : String -> Value
monthFromString str =
    Iso8601.toTime (String.slice 0 7 str ++ "-01T00:00")
        |> Result.map Month
        |> Result.toMaybe
        |> Maybe.withDefault Blank


date : Posix -> Value
date =
    Date


dateFromString : String -> Value
dateFromString str =
    Iso8601.toTime (String.slice 0 10 str ++ "T00:00")
        |> Result.map Date
        |> Result.toMaybe
        |> Maybe.withDefault Blank


time : Posix -> Value
time =
    Time


timeFromString : String -> Value
timeFromString str =
    Iso8601.toTime str
        |> Result.map Time
        |> Result.toMaybe
        |> Maybe.withDefault Blank


boolean : Bool -> Value
boolean =
    Boolean


list : List (List ( String, Value )) -> Value
list =
    List


blank : Value
blank =
    Blank


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


toInt : Value -> Maybe Int
toInt value =
    Maybe.map floor (toFloat value)


toFloat : Value -> Maybe Float
toFloat value =
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


toBool : Value -> Maybe Bool
toBool value =
    case value of
        Boolean b ->
            Just b

        _ ->
            Nothing


toList : Value -> List (List ( String, Value ))
toList value =
    case value of
        List aList ->
            aList

        _ ->
            []


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


isBlank : Value -> Bool
isBlank value =
    case value of
        Blank ->
            True

        _ ->
            False


transformString : (String -> String) -> Value -> Value
transformString f value =
    case value of
        Text val ->
            Text (f val)

        _ ->
            value


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


compare : Value -> Value -> Maybe Order
compare a b =
    Maybe.map2 Basics.compare (toFloat a) (toFloat b)
