module FormToolkit.Input exposing
    ( Input
    , text, textarea, email, password
    , integer, float
    , date, month
    , select, radio, checkbox
    , element
    , group, repeatable
    , mapIdentifier
    , Attribute
    , name, identifier, value, required, label, hint, placeholder
    , options, min, max
    , inline, noattr
    , Value
    , stringValue, integerValue, floatValue, booleanValue, blankValue
    , dateValue, monthValue, timeValue
    , fromTree, toTree
    )

{-|


# Inputs

@docs Input
@docs text, textarea, email, password
@docs integer, float
@docs date, month
@docs select, radio, checkbox
@docs element
@docs group, repeatable
@docs mapIdentifier


# Attributes

@docs Attribute
@docs name, identifier, value, required, label, hint, placeholder
@docs options, min, max
@docs inline, noattr


# Values

@docs Value
@docs stringValue, integerValue, floatValue, booleanValue, blankValue
@docs dateValue, monthValue, timeValue


# Etc

@docs fromTree, toTree

-}

import Internal.Input as Internal
import Internal.Tree as Tree exposing (Tree)
import Internal.Value as Value
import String.Extra as String
import Time exposing (Posix)


{-| TODO
-}
type Input id
    = Input (Tree (Internal.Input id))


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
element : id -> Input id
element id =
    init (Internal.Element id) []


init :
    Internal.InputType id
    -> List (Attribute id)
    -> Input id
init inputType attributes =
    Input (Tree.leaf (Internal.init inputType (unwrapAttrs attributes)))


{-| TODO
-}
mapIdentifier : (a -> b) -> Input a -> Input b
mapIdentifier func (Input tree) =
    Input (Tree.mapValues (Internal.mapIdentifier func) tree)


unwrapAttrs : List (Attribute id) -> List (Internal.Input id -> Internal.Input id)
unwrapAttrs =
    List.map (\(Attribute f) -> f)


{-| TODO
-}
type Attribute id
    = Attribute (Internal.Input id -> Internal.Input id)


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
value : Value -> Attribute id
value (Value inputValue) =
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
options : List ( String, Value ) -> Attribute id
options values =
    Attribute
        (\input ->
            { input
                | options =
                    List.map (Tuple.mapSecond (\(Value val) -> val))
                        values
            }
        )


{-| TODO
-}
min : Value -> Attribute id
min (Value val) =
    Attribute (\input -> { input | min = val })


{-| TODO
-}
max : Value -> Attribute id
max (Value val) =
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
type Value
    = Value Value.Value


{-| TODO
-}
stringValue : String -> Value
stringValue str =
    String.nonBlank str
        |> Maybe.map (Value << Value.Text)
        |> Maybe.withDefault blankValue


{-| TODO
-}
integerValue : Int -> Value
integerValue =
    Value << Value.Integer


{-| TODO
-}
floatValue : Float -> Value
floatValue =
    Value << Value.Float


{-| TODO
-}
booleanValue : Bool -> Value
booleanValue =
    Value << Value.Boolean


{-| TODO
-}
blankValue : Value
blankValue =
    Value Value.Blank


{-| TODO
-}
monthValue : Posix -> Value
monthValue =
    Value << Value.Month


{-| TODO
-}
dateValue : Posix -> Value
dateValue =
    Value << Value.Date


{-| TODO
-}
timeValue : Posix -> Value
timeValue =
    Value << Value.Time


{-| -}
fromTree : Tree (Internal.Input id) -> Input id
fromTree =
    Input


{-| -}
toTree : Input id -> Tree (Internal.Input id)
toTree (Input tree) =
    tree
