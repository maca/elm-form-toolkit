module FormToolkit.Error exposing (Error(..), toEnglish, toFieldId)

{-|

@docs Error, toEnglish, toFieldId

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


{-| Obtain the indentifier for the field corresponding to the error, if the
field has identifier.
-}
toFieldId : Error id val -> Maybe id
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
