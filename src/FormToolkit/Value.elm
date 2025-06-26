module FormToolkit.Value exposing
    ( Value(..)
    , string, int, float, bool, blank
    , date, month, time
    , toString, toBool, toFloat, toInt, toPosix
    )

{-| Value is used to set default input values of type either string, integer,
float, boolean, date, month, or time.


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


{-| -}
type Value
    = Value Internal.Value


{-| -}
string : String -> Value
string str =
    String.nonBlank str
        |> Maybe.map (Value << Internal.Text)
        |> Maybe.withDefault blank


{-| -}
int : Int -> Value
int =
    Value << Internal.fromInt


{-| -}
float : Float -> Value
float =
    Value << Internal.fromFloat


{-| -}
bool : Bool -> Value
bool =
    Value << Internal.fromBool


{-| -}
blank : Value
blank =
    Value Internal.blank


{-| -}
date : Posix -> Value
date =
    Value << Internal.Date


{-| -}
month : Posix -> Value
month =
    Value << Internal.Month


{-| -}
time : Posix -> Value
time =
    Value << Internal.Time


{-| All values except for blank have a string representation.

    toString (string "Hello") == Just "Hello"

    toString (int 42) == Just "42"

    toString blank == Nothing

-}
toString : Value -> Maybe String
toString (Value value) =
    Internal.toString value


{-| Convert boolean values to `Bool`.

    toBool (bool True) == Just True

    toBool (string "True") == Nothing

-}
toBool : Value -> Maybe Bool
toBool (Value value) =
    Internal.toBool value


{-| Convert float values to `Float`.

    toFloat (float 3.14) == Just 3.14

    toFloat (string "3.14") == Nothing

-}
toFloat : Value -> Maybe Float
toFloat (Value value) =
    Internal.toFloat value


{-| Convert int values to `Int`.

    toInt (int 42) == Just 42

    toInt (string "42") == Nothing

-}
toInt : Value -> Maybe Int
toInt (Value value) =
    Internal.toInt value


{-| Convert posix values to `Posix`.

    toPosix (time (Time.millisToPosix 0))
        == Just (Time.millisToPosix 0)

    toPosix blank == Nothing

-}
toPosix : Value -> Maybe Time.Posix
toPosix (Value value) =
    Internal.toPosix value
