module FormToolkit.Input exposing
    ( Input(..)
    , text, textarea, email, password
    , integer, float
    , date, month
    , select, radio, checkbox
    , group, repeatable
    , Attribute
    , name, identifier, value, required, label, placeholder, hint
    , options, min, max
    , noattr
    , inline, copies, repeatableMin, repeatableMax
    , Error(..), errors
    , map
    )

{-|


# Inputs

@docs Input
@docs text, textarea, email, password
@docs integer, float
@docs date, month
@docs select, radio, checkbox
@docs group, repeatable


# Attributes

@docs Attribute
@docs name, identifier, value, required, label, placeholder, hint
@docs options, min, max
@docs noattr


### Group

@docs inline, copies, repeatableMin, repeatableMax


# Errors

@docs Error, errors


# Advanced

@docs map

-}

import FormToolkit.Value as Value
import Internal.Input as Internal
import RoseTree.Tree as Tree


type alias Tree id =
    Tree.Tree (Internal.Input id (Error id))


{-| TODO
-}
type Input id
    = Input (Tree id)


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
    let
        params =
            Internal.init (Internal.Repeatable (toTree template))
                (unwrapAttrs attributes)

        children =
            List.map toTree inputs
                ++ List.repeat (params.repeatableMin - List.length inputs)
                    (toTree template)
    in
    Input (Tree.branch params children)


init : Internal.InputType id (Error id) -> List (Attribute id) -> Input id
init inputType attributes =
    Input (Tree.leaf (Internal.init inputType (unwrapAttrs attributes)))


unwrapAttrs :
    List (Attribute id)
    -> List (Internal.Input id (Error id) -> Internal.Input id (Error id))
unwrapAttrs =
    List.map (\(Attribute f) -> f)


{-| -}
toTree : Input id -> Tree id
toTree (Input tree) =
    tree


{-| TODO
-}
type Attribute id
    = Attribute (Internal.Input id (Error id) -> Internal.Input id (Error id))


{-| TODO
-}
name : String -> Attribute id
name str =
    Attribute (\input -> { input | name = Just str })


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
placeholder : String -> Attribute id
placeholder str =
    Attribute (\input -> { input | placeholder = Just str })


{-| TODO
-}
hint : String -> Attribute id
hint str =
    Attribute (\input -> { input | hint = Just str })


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
noattr : Attribute id
noattr =
    Attribute identity


{-| TODO
-}
inline : Bool -> Attribute id
inline bool =
    Attribute (\input -> { input | inline = bool })


{-| TODO
-}
copies : { addButton : String, removeButton : String } -> Attribute id
copies { addButton, removeButton } =
    Attribute
        (\input ->
            { input
                | addInputsText = addButton
                , removeInputsText = removeButton
            }
        )


{-| TODO
-}
repeatableMin : Int -> Attribute id
repeatableMin int =
    Attribute (\input -> { input | repeatableMin = int })


{-| TODO
-}
repeatableMax : Int -> Attribute id
repeatableMax int =
    Attribute (\input -> { input | repeatableMax = Just int })


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
    | ParseError (Maybe id)
    | ListError (Maybe id) { index : Int, error : Error id }
    | RepeatableHasNoName (Maybe id)
    | InputNotFound id


{-| -}
errors : Input id -> List (Error id)
errors (Input tree) =
    case Tree.children tree of
        [] ->
            Tree.value tree |> .errors

        children ->
            (Tree.value tree |> .errors)
                :: List.map (errors << Input) children
                |> List.concat


{-| TODO
-}
map : (a -> b) -> Input a -> Input b
map func (Input tree) =
    Input (Tree.mapValues (Internal.map func (mapError func)) tree)


mapError : (a -> b) -> Error a -> Error b
mapError func error =
    case error of
        ValueTooLarge id params ->
            ValueTooLarge (Maybe.map func id) params

        ValueTooSmall id params ->
            ValueTooSmall (Maybe.map func id) params

        ValueNotInRange id params ->
            ValueNotInRange (Maybe.map func id) params

        IsBlank id ->
            IsBlank (Maybe.map func id)

        ParseError id ->
            ParseError (Maybe.map func id)

        ListError id params ->
            ListError (Maybe.map func id)
                { index = params.index
                , error = mapError func params.error
                }

        RepeatableHasNoName id ->
            RepeatableHasNoName (Maybe.map func id)

        InputNotFound id ->
            InputNotFound (func id)
