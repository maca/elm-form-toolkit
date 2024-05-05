module FormToolkit.Input exposing
    ( Input(..)
    , text, textarea, email, password
    , integer, float
    , date, month
    , select, radio, checkbox
    , group, repeatable
    , elementPlaceholder
    , Attribute
    , name, identifier, value, required, label, placeholder, hint
    , options, min, max
    , inline, noattr
    , mapIdentifier
    , getValue, clear
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


# Attributes

@docs Attribute
@docs name, identifier, value, required, label, placeholder, hint
@docs options, min, max
@docs inline, noattr


# Etc

@docs mapIdentifier
@docs getValue, clear

-}

import FormToolkit.Value as Value
import Internal.Input as Internal
import Internal.Value
import RoseTree.Tree as Tree


type alias Tree id err =
    Tree.Tree (Internal.Input id err)


{-| TODO
-}
type Input id err
    = Input (Tree id err)


{-| TODO
-}
text : List (Attribute id err) -> Input id err
text =
    init Internal.Text


{-| TODO
-}
textarea : List (Attribute id err) -> Input id err
textarea =
    init Internal.TextArea


{-| TODO
-}
email : List (Attribute id err) -> Input id err
email =
    init Internal.Email


{-| TODO
-}
password : List (Attribute id err) -> Input id err
password =
    init Internal.Password


{-| TODO
-}
integer : List (Attribute id err) -> Input id err
integer =
    init Internal.Integer


{-| TODO
-}
float : List (Attribute id err) -> Input id err
float =
    init Internal.Float


{-| TODO
-}
date : List (Attribute id err) -> Input id err
date =
    init Internal.Date


{-| TODO
-}
month : List (Attribute id err) -> Input id err
month =
    init Internal.Month


{-| TODO
-}
select : List (Attribute id err) -> Input id err
select =
    init Internal.Select


{-| TODO
-}
radio : List (Attribute id err) -> Input id err
radio =
    init Internal.Radio


{-| TODO
-}
checkbox : List (Attribute id err) -> Input id err
checkbox =
    init Internal.Checkbox


{-| TODO
-}
group : List (Attribute id err) -> List (Input id err) -> Input id err
group attributes inputs =
    List.map toTree inputs
        |> Tree.branch
            (Internal.init Internal.Group (unwrapAttrs attributes))
        |> Input


{-| TODO
-}
repeatable : List (Attribute id err) -> Input id err -> List (Input id err) -> Input id err
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
elementPlaceholder : id -> Input id err
elementPlaceholder id =
    init (Internal.Element id) []


init : Internal.InputType id err -> List (Attribute id err) -> Input id err
init inputType attributes =
    Input (Tree.leaf (Internal.init inputType (unwrapAttrs attributes)))


unwrapAttrs :
    List (Attribute id err)
    -> List (Internal.Input id err -> Internal.Input id err)
unwrapAttrs =
    List.map (\(Attribute f) -> f)


{-| -}
toTree : Input id err -> Tree id err
toTree (Input tree) =
    tree


{-| TODO
-}
type Attribute id err
    = Attribute (Internal.Input id err -> Internal.Input id err)


{-| TODO
-}
name : String -> Attribute id err
name str =
    Attribute (\input -> { input | name = str })


{-| TODO
-}
identifier : id -> Attribute id err
identifier id =
    Attribute (\input -> { input | identifier = Just id })


{-| TODO
-}
value : Value.Value -> Attribute id err
value (Value.Value inputValue) =
    Attribute (\input -> { input | value = inputValue })


{-| TODO
-}
required : Bool -> Attribute id err
required bool =
    Attribute (\input -> { input | isRequired = bool })


{-| TODO
-}
label : String -> Attribute id err
label str =
    Attribute (\input -> { input | label = Just str })


{-| TODO
-}
placeholder : String -> Attribute id err
placeholder str =
    Attribute (\input -> { input | placeholder = Just str })


{-| TODO
-}
hint : String -> Attribute id err
hint str =
    Attribute (\input -> { input | hint = Just str })


{-| TODO
-}
options : List ( String, Value.Value ) -> Attribute id err
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
min : Value.Value -> Attribute id err
min (Value.Value val) =
    Attribute (\input -> { input | min = val })


{-| TODO
-}
max : Value.Value -> Attribute id err
max (Value.Value val) =
    Attribute (\input -> { input | max = val })


{-| TODO
-}
inline : Bool -> Attribute id err
inline bool =
    Attribute (\input -> { input | inline = bool })


{-| TODO
-}
noattr : Attribute id err
noattr =
    Attribute identity


{-| TODO
-}
mapIdentifier : (a -> b) -> Input a err -> Input b err
mapIdentifier func (Input tree) =
    Input (Tree.mapValues (Internal.mapIdentifier func) tree)


{-| -}
getValue : Input id err -> Value.Value
getValue (Input tree) =
    Tree.value tree |> .value |> Value.Value


{-| TODO
-}
clear : Input id err -> Input id err
clear =
    map (Tree.updateValue (Internal.update Internal.Value.blank))


map : (Tree id err -> Tree id err) -> Input id err -> Input id err
map func input =
    Input (Tree.map func (toTree input))
