module FormToolkit.Input exposing
    ( Input, Msg, update, toHtml, toView
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
    , errors
    , map
    )

{-| Provides types and functions to create and manage form inputs, including
various input types, attributes, and error handling.


# Input

@docs Input, Msg, update, toHtml, toView


# Input types

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

@docs errors


# Advanced

@docs map

-}

import FormToolkit.Decode exposing (Decoder, Error(..))
import FormToolkit.Value as Value
import FormToolkit.View as View
import Html exposing (Html)
import Internal.Input as Internal exposing (Input(..), Msg(..))
import Internal.Value
import RoseTree.Tree as Tree


type alias Tree id =
    Internal.Tree id (Error id)


{-| Represents a form input element, which can be a field or a group of fields.

Can be individual input fields (like text, email, password) or composite inputs
like groups and repeatable groups.

-}
type alias Input id =
    Internal.Input id (Error id)


{-| -}
type alias Msg id =
    Internal.Msg id


{-| TODO
-}
update :
    Msg id
    -> Decoder id a
    -> Input id
    -> ( Input id, Result (List (Error id)) a )
update msg decoder input =
    FormToolkit.Decode.validateAndDecode decoder <|
        case msg of
            InputChanged path str ->
                updateAt path (updateInput str) input

            InputChecked path bool ->
                updateAt path (updateInputWithBool bool) input

            InputFocused path ->
                updateAt path (Tree.updateValue Internal.focus) input

            InputBlured path ->
                updateAt path (Tree.updateValue Internal.blur) input

            InputsAdded path ->
                case
                    Tree.getValueAt path (toTree input)
                        |> Maybe.map .inputType
                of
                    Just (Internal.Repeatable template) ->
                        updateAt path (Tree.push template) input

                    _ ->
                        input

            InputsRemoved path ->
                Input (Tree.removeAt path (toTree input))


{-| -}
toHtml : (Msg id -> msg) -> Input id -> Html msg
toHtml tagger input =
    View.fromInput tagger input |> View.toHtml


{-| -}
toView : (Msg id -> msg) -> Input id -> View.View id msg
toView =
    View.fromInput


updateInput : String -> Tree id -> Tree id
updateInput string =
    Tree.updateValue (Internal.updateValueWithString string)


updateInputWithBool : Bool -> Tree id -> Tree id
updateInputWithBool bool =
    Tree.updateValue (Internal.updateValue (Internal.Value.fromBool bool))


updateAt : List Int -> (Tree id -> Tree id) -> Input id -> Input id
updateAt path func input =
    Input (Tree.updateAt path func (toTree input))


{-| Creates a text input field.

    text [ label "Username", placeholder "Enter your username" ]

-}
text : List (Attribute id) -> Input id
text =
    init Internal.Text


{-| Creates a textarea input field.

    textarea [ label "Comments", placeholder "Enter your comments here" ]

-}
textarea : List (Attribute id) -> Input id
textarea =
    init Internal.TextArea


{-| Creates an email input field.

    email [ label "Email", required True ]

-}
email : List (Attribute id) -> Input id
email =
    init Internal.Email


{-| Creates a password input field.

    password [ label "Password", required True ]

-}
password : List (Attribute id) -> Input id
password =
    init Internal.Password


{-| Creates an integer input field.

    int [ label "Age", min (Value.int 0), max (Value.int 120) ]

-}
int : List (Attribute id) -> Input id
int =
    init Internal.Integer


{-| Creates a floating-point number input field.

    float [ label "Price", min (Value.float 0.0) ]

-}
float : List (Attribute id) -> Input id
float =
    init Internal.Float


{-| Creates a date input field.

    date [ label "Birthdate", required True ]

-}
date : List (Attribute id) -> Input id
date =
    init Internal.Date


{-| Creates a month input field.

    month [ label "Expiry Month" ]

-}
month : List (Attribute id) -> Input id
month =
    init Internal.Month


{-| Creates a select input field (dropdown).

    select
        [ label "Language"
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
        [ label "Gender"
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

    checkbox [ label "Subscribe to newsletter" ]

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
    -> List (Internal.Attrs id (Error id) -> Internal.Attrs id (Error id))
unwrapAttrs =
    List.map (\(Attribute f) -> f)


toTree : Input id -> Tree id
toTree (Input tree) =
    tree


{-| Represents an attribute that can be applied to an input.
-}
type Attribute id
    = Attribute (Internal.Attrs id (Error id) -> Internal.Attrs id (Error id))


{-| Sets the name of an input.
-}
name : String -> Attribute id
name str =
    Attribute (\input -> { input | name = Just str })


{-| Sets the identifier of an input.
-}
identifier : id -> Attribute id
identifier id =
    Attribute (\input -> { input | identifier = Just id })


{-| Sets the value of an input.
-}
value : Value.Value -> Attribute id
value (Value.Value inputValue) =
    Attribute (\input -> { input | value = inputValue })


{-| Marks an input as required.
-}
required : Bool -> Attribute id
required bool =
    Attribute (\input -> { input | isRequired = bool })


{-| Sets the label of an input.
-}
label : String -> Attribute id
label str =
    Attribute (\input -> { input | label = Just str })


{-| Sets the placeholder text of an input.
-}
placeholder : String -> Attribute id
placeholder str =
    Attribute (\input -> { input | placeholder = Just str })


{-| Sets a hint or help text for an input.
-}
hint : String -> Attribute id
hint str =
    Attribute (\input -> { input | hint = Just str })


{-| Sets the options for a select, radio, or checkbox input.
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
-}
min : Value.Value -> Attribute id
min (Value.Value val) =
    Attribute (\input -> { input | min = val })


{-| Sets the maximum value for an input.
-}
max : Value.Value -> Attribute id
max (Value.Value val) =
    Attribute (\input -> { input | max = val })


{-| An attribute that does nothing.
-}
noattr : Attribute id
noattr =
    Attribute identity


{-| Sets whether the inputs in a group are displayed inline.
-}
inline : Bool -> Attribute id
inline bool =
    Attribute (\input -> { input | inline = bool })


{-| Sets the text for the add and remove buttons in a repeatable input.
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
-}
repeatableMin : Int -> Attribute id
repeatableMin integer =
    Attribute (\input -> { input | repeatableMin = integer })


{-| Sets the maximum number of copies for a repeatable input.
-}
repeatableMax : Int -> Attribute id
repeatableMax integer =
    Attribute (\input -> { input | repeatableMax = Just integer })


{-| Collects all errors from an input and its children.
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
