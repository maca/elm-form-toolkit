module FormToolkit.Utils exposing (formatMask)

{-| Utility functions for form formatting and text manipulation.

@docs formatMask
@docs calculateCursorPosition

-}


{-| Apply a formatting mask to input text with cursor position tracking.

Mask tokens:

  - `{d}` - matches digits

  - `{D}` - matches non-digits

  - `{w}` - matches word characters (alphanumeric + underscore)

  - `{W}` - matches non-word characters

  - Any other character is treated as a literal

    formatMask { mask = "{d}{d}{d}{d} {d}{d}{d}{d}", input = "12345678", cursorPosition = 4 }
    --> { formatted = "1234 5678", cursorPosition = 5 }

-}
formatMask : { mask : String, input : String, cursorPosition : Int } -> { formatted : String, cursorPosition : Int }
formatMask { mask, input, cursorPosition } =
    formatHelper
        { maskList = parseMask mask
        , inputList = String.toList input
        , cursor = cursorPosition
        , idx = 0
        , newCursor = Nothing
        , acc = []
        }


type MaskToken
    = Digit
    | NonDigit
    | WordChar
    | NonWordChar
    | Literal Char


formatHelper :
    { maskList : List MaskToken
    , inputList : List Char
    , idx : Int
    , cursor : Int
    , newCursor : Maybe Int
    , acc : List Char
    }
    ->
        { formatted : String
        , cursorPosition : Int
        }
formatHelper ({ maskList, inputList, idx, cursor, newCursor, acc } as params) =
    case ( maskList, inputList ) of
        ( token :: remMask, char :: remInput ) ->
            case matchingToken token char of
                Just acceptedChar ->
                    formatHelper
                        { params
                            | maskList = remMask
                            , inputList = remInput
                            , idx = idx + 1
                            , newCursor =
                                if idx == cursor then
                                    Just (List.length acc)

                                else
                                    newCursor
                            , acc = acceptedChar :: acc
                        }

                Nothing ->
                    case token of
                        Literal literalChar ->
                            formatHelper
                                { params
                                    | maskList = remMask
                                    , newCursor =
                                        if idx == cursor then
                                            Just cursor

                                        else
                                            newCursor
                                    , acc = literalChar :: acc
                                }

                        _ ->
                            formatHelper
                                { params
                                    | inputList = remInput
                                    , idx = idx + 1
                                    , newCursor = newCursor
                                }

        _ ->
            { formatted = String.fromList (List.reverse acc)
            , cursorPosition = Maybe.withDefault (List.length acc) newCursor
            }


parseMask : String -> List MaskToken
parseMask mask =
    parseMaskHelper (String.toList mask) []


parseMaskHelper : List Char -> List MaskToken -> List MaskToken
parseMaskHelper chars acc =
    case chars of
        [] ->
            List.reverse acc

        '{' :: 'd' :: '}' :: rest ->
            parseMaskHelper rest (Digit :: acc)

        '{' :: 'D' :: '}' :: rest ->
            parseMaskHelper rest (NonDigit :: acc)

        '{' :: 'w' :: '}' :: rest ->
            parseMaskHelper rest (WordChar :: acc)

        '{' :: 'W' :: '}' :: rest ->
            parseMaskHelper rest (NonWordChar :: acc)

        char :: rest ->
            parseMaskHelper rest (Literal char :: acc)


matchingToken : MaskToken -> Char -> Maybe Char
matchingToken token char =
    case token of
        Digit ->
            if Char.isDigit char then
                Just char

            else
                Nothing

        NonDigit ->
            if not (Char.isDigit char) then
                Just char

            else
                Nothing

        WordChar ->
            if Char.isAlphaNum char || char == '_' then
                Just char

            else
                Nothing

        NonWordChar ->
            if not (Char.isAlphaNum char || char == '_') then
                Just char

            else
                Nothing

        Literal patternChar ->
            if char == patternChar then
                Just char

            else
                Nothing
