module FormToolkit.Error exposing (Error(..), toEnglish, toFieldId)

{-|

@docs Error, toEnglish, toFieldId

-}

import FormToolkit.Value as Value


{-| Represents an error that occurred during decoding or validation.
-}
type Error id
    = IsBlank (Maybe id)
    | ValueTooLarge (Maybe id) { value : Value.Value, max : Value.Value }
    | ValueTooSmall (Maybe id) { value : Value.Value, min : Value.Value }
    | ValueNotInRange
        (Maybe id)
        { value : Value.Value
        , min : Value.Value
        , max : Value.Value
        }
    | NotNumber (Maybe id)
    | NotBool (Maybe id)
    | HasNoName (Maybe id)
    | PatternError (Maybe id)
    | EmailInvalid (Maybe id)
    | IsGroupNotInput (Maybe id)
    | NoOptionsProvided (Maybe id)
    | InputNotFound id
    | OneOf (Maybe id) (List (Error id))
    | ParseError (Maybe id)
    | CustomError (Maybe id) String


{-| -}
toEnglish : Error id -> String
toEnglish error =
    let
        toString =
            Value.toString >> Maybe.withDefault ""
    in
    case error of
        IsBlank _ ->
            "Should be provided"

        ValueTooLarge _ data ->
            "Should be lesser than " ++ toString data.max

        ValueTooSmall _ data ->
            "Should be greater than " ++ toString data.min

        ValueNotInRange _ data ->
            "Should be between " ++ toString data.min ++ " and " ++ toString data.max

        NotNumber _ ->
            "Must be a number"

        NotBool _ ->
            "Must be true or false"

        HasNoName _ ->
            "Couldn't parse"

        PatternError _ ->
            "Doesn't match the required pattern"

        EmailInvalid _ ->
            "Please enter a valid email address"

        IsGroupNotInput _ ->
            "A group cannot have a value but the decoder is attempting to read the value"

        NoOptionsProvided _ ->
            "No options have been provided"

        InputNotFound _ ->
            "Couldn't find an input with the given identifier"

        OneOf _ errors ->
            "All of the following failed: " ++ String.join ", " (List.map toEnglish errors)

        ParseError _ ->
            "Couldn't parse"

        CustomError _ message ->
            message


{-| Obtain the identifier for the field corresponding to the error, if the
field has identifier.
-}
toFieldId : Error id -> Maybe id
toFieldId error =
    case error of
        IsBlank maybeId ->
            maybeId

        ValueTooLarge maybeId _ ->
            maybeId

        ValueTooSmall maybeId _ ->
            maybeId

        ValueNotInRange maybeId _ ->
            maybeId

        NotNumber maybeId ->
            maybeId

        NotBool maybeId ->
            maybeId

        HasNoName maybeId ->
            maybeId

        PatternError maybeId ->
            maybeId

        EmailInvalid maybeId ->
            maybeId

        IsGroupNotInput maybeId ->
            maybeId

        NoOptionsProvided maybeId ->
            maybeId

        InputNotFound id ->
            Just id

        OneOf maybeId _ ->
            maybeId

        ParseError maybeId ->
            maybeId

        CustomError maybeId _ ->
            maybeId
