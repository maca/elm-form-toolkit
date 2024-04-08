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

@docs isBlank


# Other

encode

-}

import FormToolkit.Error as Error exposing (Error)
import Internal.Value as Internal exposing (Value)
import String.Extra as String
import Time exposing (Posix)


{-| TODO
-}
type alias Value =
    Internal.Value


{-| TODO
-}
string : String -> Value
string str =
    String.nonBlank str
        |> Maybe.map Internal.Text
        |> Maybe.withDefault Internal.Blank


{-| TODO
-}
int : Int -> Value
int =
    Internal.Integer


{-| TODO
-}
float : Float -> Value
float =
    Internal.Float


{-| TODO
-}
month : Posix -> Value
month =
    Internal.Month


{-| TODO
-}
date : Posix -> Value
date =
    Internal.Date


{-| TODO
-}
time : Posix -> Value
time =
    Internal.Time


{-| TODO
-}
boolean : Bool -> Value
boolean =
    Internal.Boolean


{-| TODO
-}
list : List (List ( String, Value )) -> Value
list =
    Internal.List


{-| TODO
-}
blank : Value
blank =
    Internal.Blank


{-| TODO
-}
isBlank : Value -> Bool
isBlank value =
    case value of
        Internal.Blank ->
            True

        _ ->
            False


{-| TODO
-}
toString : Value -> Result Error String
toString value =
    case value of
        Internal.Text val ->
            Ok val

        _ ->
            Err Error.NotString


{-| TODO
-}
toInt : Value -> Result Error Int
toInt value =
    case value of
        Internal.Integer val ->
            Ok val

        _ ->
            Err Error.NotInt


{-| TODO
-}
toFloat : Value -> Result Error Float
toFloat value =
    case value of
        Internal.Float val ->
            Ok val

        _ ->
            Err Error.NotFloat


{-| TODO
-}
toBool : Value -> Result Error Bool
toBool value =
    case value of
        Internal.Boolean val ->
            Ok val

        _ ->
            Err Error.NotBool


{-| TODO
-}
toPosix : Value -> Result Error Posix
toPosix value =
    case value of
        Internal.Month val ->
            Ok val

        Internal.Date val ->
            Ok val

        Internal.Time val ->
            Ok val

        _ ->
            Err Error.NotPosix


{-| TODO
-}
toMaybe : (Value -> Result Error a) -> Value -> Result Error (Maybe a)
toMaybe f value =
    case value of
        Internal.Blank ->
            Ok Nothing

        _ ->
            f value |> Result.map Just
