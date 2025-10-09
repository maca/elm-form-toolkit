module Internal.Utils exposing (formatMask)

{-| Utility functions for form formatting and text manipulation.

@docs formatMask

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
formatMask :
    { mask : String, input : String, cursorPosition : Int }
    ->
        { formatted : String
        , cursorPosition : Int
        , maskConsumed : Bool
        }
formatMask { mask, input, cursorPosition } =
    let
        inputLength =
            String.length input

        ( formatted, maskConsumed ) =
            doFormat mask input (String.length input)
    in
    { formatted = formatted
    , maskConsumed = maskConsumed
    , cursorPosition =
        if cursorPosition >= inputLength then
            String.length formatted

        else
            doFormat mask input cursorPosition
                |> Tuple.first
                |> String.length
    }


doFormat : String -> String -> Int -> ( String, Bool )
doFormat mask input upTo =
    formatHelper (parseMask mask) (String.toList (String.slice 0 upTo input)) []


type MaskToken
    = Digit
    | NonDigit
    | WordChar
    | NonWordChar
    | Literal Char


formatHelper : List MaskToken -> List Char -> List Char -> ( String, Bool )
formatHelper maskList inputList acc =
    case ( maskList, inputList ) of
        ( token :: mask, char :: input ) ->
            case ( matchingToken char token, token ) of
                ( True, _ ) ->
                    formatHelper mask input (char :: acc)

                ( False, Literal lit ) ->
                    let
                        nextMatches =
                            List.head mask
                                |> Maybe.map (matchingToken char)
                                |> Maybe.withDefault False
                    in
                    if nextMatches then
                        formatHelper mask (char :: input) (lit :: acc)

                    else if List.isEmpty input then
                        formatHelper mask input acc

                    else
                        formatHelper mask input (lit :: acc)

                _ ->
                    formatHelper (token :: mask) input acc

        ( [], _ ) ->
            ( String.fromList (List.reverse acc), True )

        ( _, [] ) ->
            ( String.fromList (List.reverse acc), False )


parseMask : String -> List MaskToken
parseMask mask =
    parseMaskHelper (String.toList mask) []


parseMaskHelper : List Char -> List MaskToken -> List MaskToken
parseMaskHelper chars acc =
    case chars of
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

        [] ->
            List.reverse acc


matchingToken : Char -> MaskToken -> Bool
matchingToken char token =
    case token of
        Digit ->
            Char.isDigit char

        NonDigit ->
            not (Char.isDigit char)

        WordChar ->
            Char.isAlphaNum char || char == '_'

        NonWordChar ->
            not (Char.isAlphaNum char || char == '_')

        Literal lit ->
            char == lit
