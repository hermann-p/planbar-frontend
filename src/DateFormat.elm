module DateFormat exposing (de)

import Date
import Time exposing (Month(..), Weekday(..))


de : Date.Language
de =
    { monthName = toGermanMonth
    , monthNameShort = toGermanShortMonth
    , weekdayName = toGermanWeekday
    , weekdayNameShort = toGermanShortWeekday
    , dayWithSuffix = \n -> String.fromInt n ++ "."
    }


toGermanMonth : Date.Month -> String
toGermanMonth month =
    case month of
        Jan ->
            "Januar"

        Feb ->
            "Februar"

        Mar ->
            "März"

        Apr ->
            "April"

        May ->
            "Mai"

        Jun ->
            "Juni"

        Jul ->
            "Juli"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "Oktober"

        Nov ->
            "November"

        Dec ->
            "Dezember"


toGermanShortMonth : Date.Month -> String
toGermanShortMonth month =
    toGermanMonth month |> String.slice 0 2


toGermanWeekday : Date.Weekday -> String
toGermanWeekday weekday =
    case weekday of
        Mon ->
            "Montag"

        Tue ->
            "Dienstag"

        Wed ->
            "Mittwoch"

        Thu ->
            "Donnerstag"

        Fri ->
            "Freitag"

        Sat ->
            "Samstag    "

        Sun ->
            "Sonntag"


toGermanShortWeekday : Date.Weekday -> String
toGermanShortWeekday day =
    toGermanWeekday day |> String.slice 0 3
