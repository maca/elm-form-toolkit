module Internal.Time exposing (dateMonthToHuman, dateToHuman)

import Time exposing (Month(..), Weekday(..), utc)


dateToHuman : Time.Posix -> String
dateToHuman time =
    dateToHumanHelp time |> String.join " "


dateMonthToHuman : Time.Posix -> String
dateMonthToHuman time =
    case dateToHumanHelp time of
        [ _, month, year ] ->
            month ++ " " ++ year

        _ ->
            ""


dateToHumanHelp : Time.Posix -> List String
dateToHumanHelp time =
    [ Time.toDay utc time |> String.fromInt
    , Time.toMonth utc time |> monthToHuman
    , Time.toYear utc time |> String.fromInt
    ]


monthToHuman : Month -> String
monthToHuman weekday =
    case weekday of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"
