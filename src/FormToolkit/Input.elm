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


type alias Input a =
    Tree (Input.Input a)


type Attribute a
    = Attribute (Input.Input a -> Input.Input a)



-- CREATE


text : List (Attribute a) -> Input a
text attributes =
    init Text attributes


textarea : List (Attribute a) -> Input a
textarea =
    init TextArea


email : List (Attribute a) -> Input a
email =
    init Email


password : List (Attribute a) -> Input a
password =
    init Password


integer : List (Attribute a) -> Input a
integer =
    init Integer


float : List (Attribute a) -> Input a
float =
    init Float


date : List (Attribute a) -> Input a
date =
    init Date


month : List (Attribute a) -> Input a
month =
    init Month


select : List (Attribute a) -> Input a
select =
    init Select


radio : List (Attribute a) -> Input a
radio =
    init Radio


checkbox : List (Attribute a) -> Input a
checkbox =
    init Checkbox


group : List (Attribute a) -> List (Input a) -> Input a
group attributes =
    Tree.branch
        (Input.init Group (List.map (\(Attribute f) -> f) attributes))


repeatable : Input a -> List (Attribute a) -> List (Input a) -> Input a
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


init : InputType a -> List (Attribute a) -> Input a
init inputType attributes =
    Tree.leaf (Input.init inputType (List.map (\(Attribute f) -> f) attributes))



-- ATTRIBUTES


name : String -> Attribute a
name str =
    Attribute (\input -> { input | name = str })


value : Value -> Attribute a
value inputValue =
    Attribute (\input -> { input | value = inputValue })


required : Bool -> Attribute a
required bool =
    Attribute (\input -> { input | isRequired = bool })


label : String -> Attribute a
label str =
    Attribute (\input -> { input | label = Just str })


hint : String -> Attribute a
hint str =
    Attribute (\input -> { input | hint = Just str })


placeholder : String -> Attribute a
placeholder str =
    Attribute (\input -> { input | placeholder = Just str })


options : List ( String, Value ) -> Attribute a
options dict =
    Attribute (\input -> { input | options = dict })


min : Value -> Attribute a
min val =
    Attribute (\input -> { input | min = val })


max : Value -> Attribute a
max val =
    Attribute (\input -> { input | max = val })


inline : Bool -> Attribute a
inline bool =
    Attribute (\input -> { input | inline = bool })


parser : (Value -> Maybe Value) -> Attribute a
parser f =
    Attribute (\input -> { input | parsers = f :: input.parsers })


reject : String -> Attribute a
reject string =
    parser
        (Just
            << Value.transformString
                (Regex.replace
                    (Maybe.withDefault Regex.never (Regex.fromString string))
                    (always "")
                )
        )


noattr : Attribute a
noattr =
    Attribute identity
