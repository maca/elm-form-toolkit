module FormToolkit.Error exposing (Error(..), toEnglish, toFieldId)

{-|

@docs Error, toEnglish, toFieldId

-}

import FormToolkit.Value as Value


{-| Represents an error that occurred during decoding or validation.
-}
type Error id
    = ValueTooLarge (Maybe id) { value : Value.Value, max : Value.Value }
    | ValueTooSmall (Maybe id) { value : Value.Value, min : Value.Value }
    | ValueNotInRange
        (Maybe id)
        { value : Value.Value
        , min : Value.Value
        , max : Value.Value
        }
    | IsBlank (Maybe id)
    | CustomError (Maybe id) String
    | ListError (Maybe id) { index : Int, error : Error id }
    | InputNotFound id
    | RepeatableHasNoName (Maybe id)
    | IsGroupNotInput (Maybe id)
    | NoOptionsProvided (Maybe id)
    | ParseError (Maybe id)


{-| -}
toEnglish : Error id -> String
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


{-| Obtain the identifier for the field corresponding to the error, if the
field has identifier.
-}
toFieldId : Error id -> Maybe id
toFieldId error =
    case error of
        ValueTooLarge maybeId _ ->
            maybeId

        ValueTooSmall maybeId _ ->
            maybeId

        ValueNotInRange maybeId _ ->
            maybeId

        IsGroupNotInput maybeId ->
            maybeId

        IsBlank maybeId ->
            maybeId

        CustomError maybeId _ ->
            maybeId

        ListError maybeId _ ->
            maybeId

        InputNotFound id ->
            Just id

        RepeatableHasNoName maybeId ->
            maybeId

        NoOptionsProvided maybeId ->
            maybeId

        ParseError maybeId ->
            maybeId
