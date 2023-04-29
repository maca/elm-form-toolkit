module FormToolkit.Input exposing
    ( Input, Attribute
    , text, textarea, email, password
    , integer, float
    , date, month
    , select, radio, checkbox
    , group, repeatable, element
    , name, identifier, value, required, label, hint, placeholder
    , options, min, max
    , inline, noattr
    , mapIdentifier
    , Error
    )

{-|

@docs Input, Attribute


# Init

@docs text, textarea, email, password
@docs integer, float
@docs date, month, time
@docs select, radio, checkbox
@docs group, repeatable, element


# Attributes

@docs Attribute
@docs name, identifier, value, required, label, hint, placeholder
@docs options, min, max
@docs inline, noattr
@docs mapIdentifier

-}

import FormToolkit.Error as Error
import Internal.Input as Input exposing (Input, InputType(..))
import Internal.Tree as Tree exposing (Tree)
import Internal.Value exposing (Value)


type alias Input id =
    Tree (Input.Input id)


type alias Error =
    Error.Error


type Attribute id
    = Attribute (Input.Input id -> Input.Input id)



-- CREATE


text : List (Attribute id) -> Input id
text attributes =
    init Text attributes


textarea : List (Attribute id) -> Input id
textarea =
    init TextArea


email : List (Attribute id) -> Input id
email =
    init Email


password : List (Attribute id) -> Input id
password =
    init Password


integer : List (Attribute id) -> Input id
integer =
    init Integer


float : List (Attribute id) -> Input id
float =
    init Float


date : List (Attribute id) -> Input id
date =
    init Date


month : List (Attribute id) -> Input id
month =
    init Month


select : List (Attribute id) -> Input id
select =
    init Select


radio : List (Attribute id) -> Input id
radio =
    init Radio


checkbox : List (Attribute id) -> Input id
checkbox =
    init Checkbox


group : List (Attribute id) -> List (Input id) -> Input id
group attributes =
    Tree.branch
        (Input.init Group (List.map (\(Attribute f) -> f) attributes))


repeatable : List (Attribute id) -> Input id -> List (Input id) -> Input id
repeatable attributes template inputs =
    Tree.branch
        (Input.init (Repeatable template)
            (List.map (\(Attribute f) -> f) attributes)
        )
        (if List.isEmpty inputs then
            [ template ]

         else
            inputs
        )


element : id -> Input id
element id =
    init (Element id) []


init : InputType id -> List (Attribute id) -> Input id
init inputType attributes =
    Tree.leaf (Input.init inputType (List.map (\(Attribute f) -> f) attributes))



-- ATTRIBUTES


name : String -> Attribute id
name str =
    Attribute (\input -> { input | name = str })


identifier : id -> Attribute id
identifier id =
    Attribute (\input -> { input | identifier = Just id })


value : Value -> Attribute id
value inputValue =
    Attribute (\input -> { input | value = inputValue })


required : Bool -> Attribute id
required bool =
    Attribute (\input -> { input | isRequired = bool })


label : String -> Attribute id
label str =
    Attribute (\input -> { input | label = Just str })


hint : String -> Attribute id
hint str =
    Attribute (\input -> { input | hint = Just str })


placeholder : String -> Attribute id
placeholder str =
    Attribute (\input -> { input | placeholder = Just str })


options : List ( String, Value ) -> Attribute id
options dict =
    Attribute (\input -> { input | options = dict })


min : Value -> Attribute id
min val =
    Attribute (\input -> { input | min = val })


max : Value -> Attribute id
max val =
    Attribute (\input -> { input | max = val })


inline : Bool -> Attribute id
inline bool =
    Attribute (\input -> { input | inline = bool })


noattr : Attribute id
noattr =
    Attribute identity


mapIdentifier : (a -> b) -> Input a -> Input b
mapIdentifier func =
    Tree.mapValues (Input.mapIdentifier func)
