module FormToolkit.Value exposing
    ( Value(..)
    , string, int, float, bool, blank
    , date, month, time
    , toString, toBool, toFloat, toInt, toPosix
    )

{-|


# Init

@docs Value
@docs string, int, float, bool, blank
@docs date, month, time


# Convert

@docs toString, toBool, toFloat, toInt, toPosix

-}

import Internal.Value as Internal
import String.Extra as String
import Time exposing (Posix)


{-| TODO
-}
type Value
    = Value Internal.Value


{-| TODO
-}
string : String -> Value
string str =
    String.nonBlank str
        |> Maybe.map (Value << Internal.Text)
        |> Maybe.withDefault blank


{-| TODO
-}
int : Int -> Value
int =
    Value << Internal.fromInt


{-| TODO
-}
float : Float -> Value
float =
    Value << Internal.fromFloat


{-| TODO
-}
bool : Bool -> Value
bool =
    Value << Internal.fromBool


{-| TODO
-}
blank : Value
blank =
    Value Internal.Blank


{-| TODO
-}
month : Posix -> Value
month =
    Value << Internal.Month


{-| TODO
-}
date : Posix -> Value
date =
    Value << Internal.Date


{-| TODO
-}
time : Posix -> Value
time =
    Value << Internal.Time


{-| TODO
-}
toString : Value -> Maybe String
toString (Value value) =
    Internal.toString value |> Result.toMaybe


{-| TODO
-}
toInt : Value -> Maybe Int
toInt (Value value) =
    Internal.toInt value |> Result.toMaybe


{-| TODO
-}
toFloat : Value -> Maybe Float
toFloat (Value value) =
    Internal.toFloat value |> Result.toMaybe


{-| TODO
-}
toBool : Value -> Maybe Bool
toBool (Value value) =
    Internal.toBool value |> Result.toMaybe


{-| TODO
-}
toPosix : Value -> Maybe Time.Posix
toPosix (Value value) =
    Internal.toPosix value |> Result.toMaybe
