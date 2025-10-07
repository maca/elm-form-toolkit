module FormToolkit.Utils exposing
    ( formatMask
    , calculateCursorPosition
    )

{-| Utility functions for form formatting and text manipulation.

@docs formatMask
@docs calculateCursorPosition

-}


{-| Calculate a cursor position by comparing previous and edited input text.

    calculateCursorPosition "12345" "1234 5" 4
    --> 4

    -- with the cursor on pos 5 the user inserts 6
    calculateCursorPosition "12345" "1234 56" 5
    --> 6

    calculateCursorPosition "1234567890" "12345678 90" 6
    --> 6

-}
calculateCursorPosition : String -> String -> Int -> Int
calculateCursorPosition original formatted cursorPos =
    matchChars
        (String.toList (String.left cursorPos original))
        (String.toList formatted)
        0


matchChars : List Char -> List Char -> Int -> Int
matchChars source target targetPos =
    case source of
        [] ->
            targetPos

        sourceChar :: remainingSource ->
            case target of
                [] ->
                    targetPos

                targetChar :: remainingTarget ->
                    if sourceChar == targetChar then
                        matchChars remainingSource remainingTarget (targetPos + 1)

                    else
                        matchChars source remainingTarget (targetPos + 1)


type MaskToken
    = Digit
    | NonDigit
    | WordChar
    | NonWordChar
    | Literal Char


{-| Apply a formatting mask to input text.

Mask tokens:

  - `{d}` - matches digits
  - `{D}` - matches non-digits
  - `{w}` - matches word characters (alphanumeric + underscore)
  - `{W}` - matches non-word characters
  - Any other character is treated as a literal

Example: `formatMask "{d}{d}{d}{d} {d}{d}{d}{d}" "12345678"` returns `"1234 5678"`

-}
formatMask : String -> String -> String
formatMask mask input =
    formatHelper (parseMask mask) (String.toList input) []
        |> String.fromList


formatHelper : List MaskToken -> List Char -> List Char -> List Char
formatHelper maskList inputList acc =
    case ( maskList, inputList ) of
        ( [], _ ) ->
            List.reverse acc

        ( _, [] ) ->
            List.reverse acc

        ( token :: remMask, char :: remInput ) ->
            case matchingToken token char of
                Just acceptedChar ->
                    formatHelper remMask remInput (acceptedChar :: acc)

                Nothing ->
                    case token of
                        Literal literalChar ->
                            formatHelper remMask inputList (literalChar :: acc)

                        _ ->
                            formatHelper maskList remInput acc



-- case ( token, remMask ) of
--     ( Literal litChar, nextToken :: nextRestMask ) ->
--         case matchingToken nextToken char of
--             Just _ ->
--                 formatHelper nextRestMask inputList (litChar :: acc)
--             Nothing ->
--                 formatHelper maskList remInput acc
--     _ ->
--         formatHelper maskList remInput acc


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
