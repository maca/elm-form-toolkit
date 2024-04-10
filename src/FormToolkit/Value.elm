module FormToolkit.Value exposing
    ( Value(..)
    , string, integer, float, boolean, blank
    , date, month, time
    , toString, toBoolean, toFloat, toInt, toPosix
    )

{-|


# Init

@docs Value
@docs string, integer, float, boolean, blank
@docs date, month, time


# Convert

@docs toString, toBoolean, toFloat, toInt, toPosix

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
integer : Int -> Value
integer =
    Value << Internal.Integer


{-| TODO
-}
float : Float -> Value
float =
    Value << Internal.Float


{-| TODO
-}
boolean : Bool -> Value
boolean =
    Value << Internal.Boolean


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
toBoolean : Value -> Maybe Bool
toBoolean (Value value) =
    Internal.toBoolean value |> Result.toMaybe


{-| TODO
-}
toPosix : Value -> Maybe Time.Posix
toPosix (Value value) =
    Internal.toPosix value |> Result.toMaybe
