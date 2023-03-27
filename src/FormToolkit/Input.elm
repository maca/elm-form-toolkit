module FormToolkit.Input exposing
    ( Input, Attribute
    , text, textarea, email, password
    , integer, float
    , date, month
    , select, radio, checkbox
    , group, repeatable
    , name, value, required, label, hint, placeholder
    , options, min, max
    , inline, noattr
    , reject
    )

{-|

@docs Input, Attribute


# Create

@docs text, textarea, email, password
@docs integer, float
@docs date, month, time
@docs select, radio, checkbox
@docs group, repeatable, help


# Attributes

@docs Attribute
@docs name, value, required, label, hint, placeholder, help
@docs options, min, max
@docs inline, noattr
@docs reject

-}

import FormToolkit.Value as Value exposing (Value)
import Internal.Input as Input exposing (Input, InputType(..))
import Internal.Tree as Tree exposing (Tree)
import Regex


type alias Input =
    Tree Input.Input


type Attribute
    = Attribute (Input.Input -> Input.Input)



-- CREATE


text : List Attribute -> Input
text attributes =
    init Text attributes


textarea : List Attribute -> Input
textarea =
    init TextArea


email : List Attribute -> Input
email =
    init Email


password : List Attribute -> Input
password =
    init Password


integer : List Attribute -> Input
integer =
    init Integer


float : List Attribute -> Input
float =
    init Float


date : List Attribute -> Input
date =
    init Date


month : List Attribute -> Input
month =
    init Month


select : List Attribute -> Input
select =
    init Select


radio : List Attribute -> Input
radio =
    init Radio


checkbox : List Attribute -> Input
checkbox =
    init Checkbox


group : List Attribute -> List Input -> Input
group attributes =
    Tree.branch
        (Input.init Group (List.map (\(Attribute f) -> f) attributes))


repeatable : Input -> List Attribute -> List Input -> Input
repeatable template attributes inputs =
    Tree.branch
        (Input.init (Repeatable template)
            (List.map (\(Attribute f) -> f) attributes)
        )
        (if List.isEmpty inputs then
            [ template ]

         else
            inputs
        )


init : InputType -> List Attribute -> Input
init inputType attributes =
    Tree.leaf (Input.init inputType (List.map (\(Attribute f) -> f) attributes))



-- ATTRIBUTES


name : String -> Attribute
name str =
    Attribute (\input -> { input | name = str })


value : Value -> Attribute
value inputValue =
    Attribute (\input -> { input | value = inputValue })


required : Bool -> Attribute
required bool =
    Attribute (\input -> { input | isRequired = bool })


label : String -> Attribute
label str =
    Attribute (\input -> { input | label = Just str })


hint : String -> Attribute
hint str =
    Attribute (\input -> { input | hint = Just str })


placeholder : String -> Attribute
placeholder str =
    Attribute (\input -> { input | placeholder = Just str })


options : List ( String, Value ) -> Attribute
options dict =
    Attribute (\input -> { input | options = dict })


min : Value -> Attribute
min val =
    Attribute (\input -> { input | min = val })


max : Value -> Attribute
max val =
    Attribute (\input -> { input | max = val })


inline : Bool -> Attribute
inline bool =
    Attribute (\input -> { input | inline = bool })


parser : (Value -> Maybe Value) -> Attribute
parser f =
    Attribute (\input -> { input | parsers = f :: input.parsers })


reject : String -> Attribute
reject string =
    parser
        (Just
            << Value.transformString
                (Regex.replace
                    (Maybe.withDefault Regex.never (Regex.fromString string))
                    (always "")
                )
        )


noattr : Attribute
noattr =
    Attribute identity
