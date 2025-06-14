module FormToolkit.Value exposing
    ( Value(..)
    , string, int, float, bool, blank
    , date, month, time
    , custom
    , toString, toBool, toFloat, toInt, toPosix
    , toCustom
    , mapCustom
    )

{-| Value is used to set default input values of type either string, integer,
float, boolean, date, month, or time.


# Init

@docs Value
@docs string, int, float, bool, blank
@docs date, month, time
@docs custom


# Convert

@docs toString, toBool, toFloat, toInt, toPosix
@docs toCustom


# Map

@docs mapCustom

-}

import Internal.Value as Internal
import String.Extra as String
import Time exposing (Posix)


{-| -}
type Value val
    = Value (Internal.Value val)


{-| -}
string : String -> Value val
string str =
    String.nonBlank str
        |> Maybe.map (Value << Internal.Text)
        |> Maybe.withDefault blank


{-| -}
int : Int -> Value val
int =
    Value << Internal.fromInt


{-| -}
float : Float -> Value val
float =
    Value << Internal.fromFloat


{-| -}
bool : Bool -> Value val
bool =
    Value << Internal.fromBool


{-| -}
blank : Value val
blank =
    Value Internal.blank


{-| -}
date : Posix -> Value val
date =
    Value << Internal.Date


{-| -}
month : Posix -> Value val
month =
    Value << Internal.Month


{-| -}
time : Posix -> Value val
time =
    Value << Internal.Time


{-| -}
custom : val -> Value val
custom =
    Value << Internal.Custom


{-| All values except for blank have a string representation.

    toString (string "Hello") == Just "Hello"

    toString (int 42) == Just "42"

    toString blank == Nothing

-}
toString : Value val -> Maybe String
toString (Value value) =
    Internal.toString value


{-| Convert boolean values to `Bool`.

    toBool (bool True) == Just True

    toBool (string "True") == Nothing

-}
toBool : Value val -> Maybe Bool
toBool (Value value) =
    Internal.toBool value


{-| Convert float values to `Float`.

    toFloat (float 3.14) == Just 3.14

    toFloat (string "3.14") == Nothing

-}
toFloat : Value val -> Maybe Float
toFloat (Value value) =
    Internal.toFloat value


{-| Convert int values to `Int`.

    toInt (int 42) == Just 42

    toInt (string "42") == Nothing

-}
toInt : Value val -> Maybe Int
toInt (Value value) =
    Internal.toInt value


{-| Convert posix values to `Posix`.

    toPosix (time (Time.millisToPosix 0))
        == Just (Time.millisToPosix 0)

    toPosix blank == Nothing

-}
toPosix : Value val -> Maybe Time.Posix
toPosix (Value value) =
    Internal.toPosix value


{-| -}
toCustom : Value val -> Maybe val
toCustom (Value value) =
    Internal.toCustom value


{-| Apply a function to a custom value preserving other kind of values.

    mapCustom (Tuple.mapFirst not) (custom ( True, False ))
        == custom ( False, False )

    mapCustom (Tuple.mapFirst not) (int 1)
        == int 1

-}
mapCustom : (val1 -> val2) -> Value val1 -> Value val2
mapCustom fun (Value value) =
    Value (Internal.mapCustom fun value)
