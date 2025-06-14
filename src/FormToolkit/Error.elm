module FormToolkit.Error exposing (Error(..), toEnglish)

{-|

@docs Error, toEnglish

-}

import FormToolkit.Value as Value


{-| Represents an error that occurred during decoding or validation.
-}
type Error id val
    = ValueTooLarge (Maybe id) { value : Value.Value val, max : Value.Value val }
    | ValueTooSmall (Maybe id) { value : Value.Value val, min : Value.Value val }
    | ValueNotInRange
        (Maybe id)
        { value : Value.Value val
        , min : Value.Value val
        , max : Value.Value val
        }
    | IsBlank (Maybe id)
    | CustomError (Maybe id) String
    | ListError (Maybe id) { index : Int, error : Error id val }
    | InputNotFound id
    | RepeatableHasNoName (Maybe id)
    | IsGroupNotInput (Maybe id)
    | NoOptionsProvided (Maybe id)
    | ParseError (Maybe id)


{-| -}
toEnglish : Error id val -> String
toEnglish error =
    let
        toString =
            Value.toString >> Maybe.withDefault ""
    in
    case error of
        ValueTooLarge _ data ->
            "Should be lesser than " ++ toString data.max

        ValueTooSmall _ data ->
            "Should be greater than " ++ toString data.min

        ValueNotInRange _ data ->
            "Should be between " ++ toString data.min ++ " and " ++ toString data.max

        IsBlank _ ->
            "Should be provided"

        IsGroupNotInput _ ->
            "A group cannot have a value but the decoder is attempting to read the value"

        NoOptionsProvided _ ->
            "No options have been provided"

        CustomError _ message ->
            message

        _ ->
            "Couldn't parse"
