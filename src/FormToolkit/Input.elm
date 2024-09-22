module FormToolkit.Input exposing
    ( Input(..)
    , text, textarea, email, password
    , int, float
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

{-| Provides types and functions to create and manage form inputs, including
various input types, attributes, and error handling.


# Inputs

@docs Input
@docs text, textarea, email, password
@docs int, float
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


{-| Represents a form input element, which can be a field or a group of fields.

Can be individual input fields (like text, email, password) or composite inputs
like groups and repeatable groups.

-}
type Input id
    = Input (Tree id)


{-| Creates a text input field.

    text [ name "username", placeholder "Enter your username" ]

-}
text : List (Attribute id) -> Input id
text =
    init Internal.Text


{-| Creates a textarea input field.

    textarea [ name "comments", placeholder "Enter your comments here" ]

-}
textarea : List (Attribute id) -> Input id
textarea =
    init Internal.TextArea


{-| Creates an email input field.

    email [ name "email", required True ]

-}
email : List (Attribute id) -> Input id
email =
    init Internal.Email


{-| Creates a password input field.

    password [ name "password", required True ]

-}
password : List (Attribute id) -> Input id
password =
    init Internal.Password


{-| Creates an integer input field.

    int [ name "age", min (Value.int 0), max (Value.int 120) ]

-}
int : List (Attribute id) -> Input id
int =
    init Internal.Integer


{-| Creates a floating-point number input field.

    float [ name "price", min (Value.float 0.0) ]

-}
float : List (Attribute id) -> Input id
float =
    init Internal.Float


{-| Creates a date input field.

    date [ name "birthdate", required True ]

-}
date : List (Attribute id) -> Input id
date =
    init Internal.Date


{-| Creates a month input field.

    month [ name "expiryMonth" ]

-}
month : List (Attribute id) -> Input id
month =
    init Internal.Month


{-| Creates a select input field (dropdown).

    select
        [ name "language"
        , options
            [ ( "Español", Value.string "ES" )
            , ( "English", Value.string "EN" )
            ]
        ]

-}
select : List (Attribute id) -> Input id
select =
    init Internal.Select


{-| Creates a radio button input field.

    radio
        [ name "gender"
        , options
            [ ( "Male", Value.string "male" )
            , ( "Female", Value.string "female" )
            ]
        ]

-}
radio : List (Attribute id) -> Input id
radio =
    init Internal.Radio


{-| Creates a checkbox input field.

    checkbox [ name "subscribe", label "Subscribe to newsletter" ]

-}
checkbox : List (Attribute id) -> Input id
checkbox =
    init Internal.Checkbox


{-| Creates a group of inputs.

Groups multiple inputs together.

    group []
        [ text [ name "firstName", label "First Name" ]
        , text [ name "lastName", label "Last Name" ]
        ]

-}
group : List (Attribute id) -> List (Input id) -> Input id
group attributes inputs =
    List.map toTree inputs
        |> Tree.branch
            (Internal.init Internal.Group (unwrapAttrs attributes))
        |> Input


{-| Creates a repeatable group of inputs.

Allows inputs to be repeated multiple times.

    repeatable [ name "emails", repeatableMin 1, repeatableMax 5 ]
        (text [ placeholder "Enter email address" ])
        []

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


toTree : Input id -> Tree id
toTree (Input tree) =
    tree


{-| Represents an attribute that can be applied to an input.
-}
type Attribute id
    = Attribute (Internal.Input id (Error id) -> Internal.Input id (Error id))


{-| Sets the name of an input.

    name "email"

-}
name : String -> Attribute id
name str =
    Attribute (\input -> { input | name = Just str })


{-| Sets the identifier of an input.

    identifier EmailField

-}
identifier : id -> Attribute id
identifier id =
    Attribute (\input -> { input | identifier = Just id })


{-| Sets the value of an input.

    value (Value.string "Default Text")

-}
value : Value.Value -> Attribute id
value (Value.Value inputValue) =
    Attribute (\input -> { input | value = inputValue })


{-| Marks an input as required.

    required True

-}
required : Bool -> Attribute id
required bool =
    Attribute (\input -> { input | isRequired = bool })


{-| Sets the label of an input.

    label "First Name"

-}
label : String -> Attribute id
label str =
    Attribute (\input -> { input | label = Just str })


{-| Sets the placeholder text of an input.

    placeholder "Enter your email"

-}
placeholder : String -> Attribute id
placeholder str =
    Attribute (\input -> { input | placeholder = Just str })


{-| Sets a hint or help text for an input.

    hint "We will not share your email"

-}
hint : String -> Attribute id
hint str =
    Attribute (\input -> { input | hint = Just str })


{-| Sets the options for a select, radio, or checkbox input.

    options [ ( "Option 1", Value.string "1" ), ( "Option 2", Value.string "2" ) ]

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


{-| Sets the minimum value for an input.

    min (Value.int 1)

-}
min : Value.Value -> Attribute id
min (Value.Value val) =
    Attribute (\input -> { input | min = val })


{-| Sets the maximum value for an input.

    max (Value.int 10)

-}
max : Value.Value -> Attribute id
max (Value.Value val) =
    Attribute (\input -> { input | max = val })


{-| An attribute that does nothing.

Can be used when an attribute is required but no specific attribute is needed.

-}
noattr : Attribute id
noattr =
    Attribute identity


{-| Sets whether the inputs in a group are displayed inline.

    inline True

-}
inline : Bool -> Attribute id
inline bool =
    Attribute (\input -> { input | inline = bool })


{-| Sets the text for the add and remove buttons in a repeatable input.

    copies { addButton = "Add another", removeButton = "Remove" }

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


{-| Sets the minimum number of copies for a repeatable input.

    repeatableMin 1

-}
repeatableMin : Int -> Attribute id
repeatableMin integer =
    Attribute (\input -> { input | repeatableMin = integer })


{-| Sets the maximum number of copies for a repeatable input.

    repeatableMax 5

-}
repeatableMax : Int -> Attribute id
repeatableMax integer =
    Attribute (\input -> { input | repeatableMax = Just integer })


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
    | CustomError String


{-| Collects all errors from an input and its children.

    errors myInput

-}
errors : Input id -> List (Error id)
errors (Input tree) =
    case Tree.children tree of
        [] ->
            Tree.value tree |> .errors

        children ->
            (Tree.value tree |> .errors)
                :: List.map (errors << Input) children
                |> List.concat


{-| Transforms identifiers or errors in an input.

    map String.toUpper myInput

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

        CustomError err ->
            CustomError err
