module FormToolkit.Input exposing
    ( Input
    , text, textarea, email, password
    , integer, float
    , date, month
    , select, radio, checkbox
    , group, repeatable
    , elementPlaceholder
    , mapIdentifier
    , Attribute
    , name, identifier, value, required, label, hint, placeholder
    , options, min, max
    , inline, noattr
    , Error(..), error, check, errorToEnglish
    , fromTree, toTree
    )

{-|


# Inputs

@docs Input
@docs text, textarea, email, password
@docs integer, float
@docs date, month
@docs select, radio, checkbox
@docs group, repeatable
@docs elementPlaceholder
@docs mapIdentifier


# Attributes

@docs Attribute
@docs name, identifier, value, required, label, hint, placeholder
@docs options, min, max
@docs inline, noattr


# Validation

@docs Error, error, check, errorToEnglish


# Etc

@docs fromTree, toTree

-}

import FormToolkit.Value as Value
import Internal.Input as Internal
import Internal.Tree as Tree exposing (Tree)
import Internal.Value
import String.Extra as String
import Time exposing (Posix)


{-| TODO
-}
type Input id
    = Input (Tree (Internal.Input id Error))


{-| TODO
-}
text : List (Attribute id) -> Input id
text =
    init Internal.Text


{-| TODO
-}
textarea : List (Attribute id) -> Input id
textarea =
    init Internal.TextArea


{-| TODO
-}
email : List (Attribute id) -> Input id
email =
    init Internal.Email


{-| TODO
-}
password : List (Attribute id) -> Input id
password =
    init Internal.Password


{-| TODO
-}
integer : List (Attribute id) -> Input id
integer =
    init Internal.Integer


{-| TODO
-}
float : List (Attribute id) -> Input id
float =
    init Internal.Float


{-| TODO
-}
date : List (Attribute id) -> Input id
date =
    init Internal.Date


{-| TODO
-}
month : List (Attribute id) -> Input id
month =
    init Internal.Month


{-| TODO
-}
select : List (Attribute id) -> Input id
select =
    init Internal.Select


{-| TODO
-}
radio : List (Attribute id) -> Input id
radio =
    init Internal.Radio


{-| TODO
-}
checkbox : List (Attribute id) -> Input id
checkbox =
    init Internal.Checkbox


{-| TODO
-}
group : List (Attribute id) -> List (Input id) -> Input id
group attributes inputs =
    List.map toTree inputs
        |> Tree.branch
            (Internal.init Internal.Group (unwrapAttrs attributes))
        |> Input


{-| TODO
-}
repeatable : List (Attribute id) -> Input id -> List (Input id) -> Input id
repeatable attributes template inputs =
    Input <|
        Tree.branch
            (Internal.init (Internal.Repeatable (toTree template))
                (unwrapAttrs attributes)
            )
            (if List.isEmpty inputs then
                [ toTree template ]

             else
                List.map toTree inputs
            )


{-| TODO
-}
elementPlaceholder : id -> Input id
elementPlaceholder id =
    init (Internal.Element id) []


init :
    Internal.InputType id Error
    -> List (Attribute id)
    -> Input id
init inputType attributes =
    Input (Tree.leaf (Internal.init inputType (unwrapAttrs attributes)))


{-| TODO
-}
mapIdentifier : (a -> b) -> Input a -> Input b
mapIdentifier func (Input tree) =
    Input (Tree.mapValues (Internal.mapIdentifier func) tree)


unwrapAttrs :
    List (Attribute id)
    -> List (Internal.Input id Error -> Internal.Input id Error)
unwrapAttrs =
    List.map (\(Attribute f) -> f)


{-| TODO
-}
type Attribute id
    = Attribute (Internal.Input id Error -> Internal.Input id Error)


{-| TODO
-}
name : String -> Attribute id
name str =
    Attribute (\input -> { input | name = str })


{-| TODO
-}
identifier : id -> Attribute id
identifier id =
    Attribute (\input -> { input | identifier = Just id })


{-| TODO
-}
value : Value.Value -> Attribute id
value (Value.Value inputValue) =
    Attribute (\input -> { input | value = inputValue })


{-| TODO
-}
required : Bool -> Attribute id
required bool =
    Attribute (\input -> { input | isRequired = bool })


{-| TODO
-}
label : String -> Attribute id
label str =
    Attribute (\input -> { input | label = Just str })


{-| TODO
-}
hint : String -> Attribute id
hint str =
    Attribute (\input -> { input | hint = Just str })


{-| TODO
-}
placeholder : String -> Attribute id
placeholder str =
    Attribute (\input -> { input | placeholder = Just str })


{-| TODO
-}
options : List ( String, Value.Value ) -> Attribute id
options values =
    Attribute
        (\input ->
            { input
                | options =
                    List.map (Tuple.mapSecond (\(Value.Value val) -> val))
                        values
            }
        )


{-| TODO
-}
min : Value.Value -> Attribute id
min (Value.Value val) =
    Attribute (\input -> { input | min = val })


{-| TODO
-}
max : Value.Value -> Attribute id
max (Value.Value val) =
    Attribute (\input -> { input | max = val })


{-| TODO
-}
inline : Bool -> Attribute id
inline bool =
    Attribute (\input -> { input | inline = bool })


{-| TODO
-}
noattr : Attribute id
noattr =
    Attribute identity


{-| TODO
-}
type Error
    = TooLarge { actual : Value.Value, max : Value.Value }
    | TooSmall { actual : Value.Value, min : Value.Value }
    | NotInRange { actual : Value.Value, min : Value.Value, max : Value.Value }
    | IsBlank
    | NotString Value.Value
    | NotInt Value.Value
    | NotFloat Value.Value
    | NotBool Value.Value
    | NotPosix Value.Value


{-| TODO
-}
error : Internal.Input id Error -> Maybe Error
error { status } =
    case status of
        Internal.WithError err ->
            Just err

        _ ->
            Nothing


{-| TODO
-}
check : Internal.Input id Error -> Result Error Value.Value
check input =
    checkRequired input
        |> Result.andThen (\_ -> checkInRange input)


checkRequired : Internal.Input id Error -> Result Error Value.Value
checkRequired input =
    if input.isRequired && Internal.Value.isBlank input.value then
        Err IsBlank

    else
        Ok (Value.Value input.value)


checkInRange : Internal.Input id Error -> Result Error Value.Value
checkInRange input =
    let
        actual =
            Value.Value input.value
    in
    case
        ( Internal.Value.compare input.value input.min
        , Internal.Value.compare input.value input.max
        )
    of
        ( Just LT, Just _ ) ->
            Err
                (NotInRange
                    { actual = actual
                    , min = Value.Value input.min
                    , max = Value.Value input.max
                    }
                )

        ( Just _, Just GT ) ->
            Err
                (NotInRange
                    { actual = actual
                    , min = Value.Value input.min
                    , max = Value.Value input.max
                    }
                )

        ( Just LT, Nothing ) ->
            Err (TooSmall { actual = actual, min = Value.Value input.min })

        ( Nothing, Just GT ) ->
            Err (TooLarge { actual = actual, max = Value.Value input.max })

        _ ->
            Ok actual


{-| TODO
-}
errorToEnglish : Error -> String
errorToEnglish err =
    let
        toString =
            Value.toString
                >> Maybe.withDefault ""
    in
    case err of
        TooLarge data ->
            "Should be lesser than " ++ toString data.max

        TooSmall data ->
            "Should be greater than " ++ toString data.min

        NotInRange data ->
            "Should be between " ++ toString data.min ++ " and " ++ toString data.max

        IsBlank ->
            "Should be provided"

        _ ->
            "It's not valid"


{-| -}
fromTree : Tree (Internal.Input id Error) -> Input id
fromTree =
    Input


{-| -}
toTree : Input id -> Tree (Internal.Input id Error)
toTree (Input tree) =
    tree
