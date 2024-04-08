module FormToolkit.Input exposing
    ( Input
    , text, textarea, email, password
    , integer, float
    , date, month
    , select, radio, checkbox
    , group, repeatable, element
    , Attribute
    , name, identifier, value, required, label, hint, placeholder
    , options, min, max
    , inline, noattr
    , mapIdentifier
    )

{-|


# Inputs

@docs Input
@docs text, textarea, email, password
@docs integer, float
@docs date, month
@docs select, radio, checkbox
@docs group, repeatable, element


# Attributes

@docs Attribute
@docs name, identifier, value, required, label, hint, placeholder
@docs options, min, max
@docs inline, noattr
@docs mapIdentifier

-}

import Internal.Input as Input
import Internal.Tree as Tree exposing (Tree)
import Internal.Value exposing (Value)


{-| TODO
-}
type alias Input id =
    Tree (Input.Input id)


{-| TODO
-}
text : List (Attribute id) -> Input id
text =
    init Input.Text


{-| TODO
-}
textarea : List (Attribute id) -> Input id
textarea =
    init Input.TextArea


{-| TODO
-}
email : List (Attribute id) -> Input id
email =
    init Input.Email


{-| TODO
-}
password : List (Attribute id) -> Input id
password =
    init Input.Password


{-| TODO
-}
integer : List (Attribute id) -> Input id
integer =
    init Input.Integer


{-| TODO
-}
float : List (Attribute id) -> Input id
float =
    init Input.Float


{-| TODO
-}
date : List (Attribute id) -> Input id
date =
    init Input.Date


{-| TODO
-}
month : List (Attribute id) -> Input id
month =
    init Input.Month


{-| TODO
-}
select : List (Attribute id) -> Input id
select =
    init Input.Select


{-| TODO
-}
radio : List (Attribute id) -> Input id
radio =
    init Input.Radio


{-| TODO
-}
checkbox : List (Attribute id) -> Input id
checkbox =
    init Input.Checkbox


{-| TODO
-}
group : List (Attribute id) -> List (Input id) -> Input id
group attributes =
    Tree.branch (Input.init Input.Group (List.map toFunc attributes))


{-| TODO
-}
repeatable : List (Attribute id) -> Input id -> List (Input id) -> Input id
repeatable attributes template inputs =
    Tree.branch
        (Input.init (Input.Repeatable template)
            (List.map toFunc attributes)
        )
        (if List.isEmpty inputs then
            [ template ]

         else
            inputs
        )


{-| TODO
-}
element : id -> Input id
element id =
    init (Input.Element id) []


init :
    Input.InputType id
    -> List (Attribute id)
    -> Input id
init inputType attributes =
    Tree.leaf
        (Input.init inputType (List.map toFunc attributes))


{-| TODO
-}
type Attribute id
    = Attribute (Input.Input id -> Input.Input id)


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
value inputValue =
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
options dict =
    Attribute (\input -> { input | options = dict })


{-| TODO
-}
min : Value -> Attribute id
min val =
    Attribute (\input -> { input | min = val })


{-| TODO
-}
max : Value -> Attribute id
max val =
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
mapIdentifier : (a -> b) -> Input a -> Input b
mapIdentifier func =
    Tree.mapValues (Input.mapIdentifier func)


{-| Don't worry about it
-}
toFunc : Attribute id -> (Input.Input id -> Input.Input id)
toFunc (Attribute func) =
    func
