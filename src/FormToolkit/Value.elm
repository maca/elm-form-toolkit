module FormToolkit.Value exposing
    ( Value
    , string, int, float, date, month, time, boolean, list, blank
    , toString, toInt, toFloat, toBool, toPosix, toMaybe
    , isBlank
    )

{-|

@docs Value


# Create

@docs string, int, float, date, month, time, boolean, list, blank


# Convert

@docs toString, toInt, toFloat, toBool, toPosix, toMaybe


# Other

@docs isBlank, compare


# Other

encode

-}

import FormToolkit.Error as Error exposing (Error)
import Internal.Value as Internal exposing (Value(..))
import String.Extra as String
import Time exposing (Posix)


type alias Value =
    Internal.Value


string : String -> Value
string str =
    String.nonBlank str
        |> Maybe.map Text
        |> Maybe.withDefault Blank


int : Int -> Value
int =
    Integer


float : Float -> Value
float =
    Float


month : Posix -> Value
month =
    Month


date : Posix -> Value
date =
    Date


time : Posix -> Value
time =
    Time


boolean : Bool -> Value
boolean =
    Boolean


list : List (List ( String, Value )) -> Value
list =
    List


blank : Value
blank =
    Blank


isBlank : Value -> Bool
isBlank value =
    case value of
        Blank ->
            True

        _ ->
            False


toString : Value -> Result Error String
toString value =
    case value of
        Text val ->
            Ok val

        _ ->
            Err Error.NotString


toInt : Value -> Result Error Int
toInt value =
    case value of
        Integer val ->
            Ok val

        _ ->
            Err Error.NotInt


toFloat : Value -> Result Error Float
toFloat value =
    case value of
        Float val ->
            Ok val

        _ ->
            Err Error.NotFloat


toBool : Value -> Result Error Bool
toBool value =
    case value of
        Boolean val ->
            Ok val

        _ ->
            Err Error.NotBool


toPosix : Value -> Result Error Posix
toPosix value =
    case value of
        Month val ->
            Ok val

        Date val ->
            Ok val

        Time val ->
            Ok val

        _ ->
            Err Error.NotPosix


toMaybe : (Value -> Result Error a) -> Value -> Result Error (Maybe a)
toMaybe f value =
    case value of
        Blank ->
            Ok Nothing

        _ ->
            f value |> Result.map Just
