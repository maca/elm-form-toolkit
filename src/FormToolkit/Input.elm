module FormToolkit.Input exposing
    ( Input, Msg, update, toHtml
    , text, textarea, email, password, strictAutocomplete
    , int, float
    , date, month
    , select, radio, checkbox
    , group, repeatable
    , Attribute
    , name, identifier, value, required, label, placeholder, hint
    , options, stringOptions, min, max
    , noattr
    , copies, repeatableMin, repeatableMax
    , updateAttributes
    , errors
    , map, mapValues
    )

{-| Provides types and functions to create and manage form inputs, including
various input types, attributes, and updating and rendering.


# Input

@docs Input, Msg, update, toHtml


# Input types

@docs text, textarea, email, password, strictAutocomplete
@docs int, float
@docs date, month
@docs select, radio, checkbox
@docs group, repeatable


# Attributes

@docs Attribute
@docs name, identifier, value, required, label, placeholder, hint
@docs options, stringOptions, min, max
@docs noattr


### Group

@docs copies, repeatableMin, repeatableMax


# Update attributes

@docs updateAttributes


# Errors

@docs errors


# Mapping and composition

@docs map, mapValues

-}

import FormToolkit.Decode exposing (Decoder, Error(..))
import FormToolkit.Value as Value
import Html exposing (Html)
import Internal.Input as Internal exposing (Attributes, Input, Msg(..))
import Internal.View as View
import RoseTree.Tree as Tree


{-| Represents a form input element, which can be a field or a group of fields.

Can be individual input fields (like text, email, password) or composite inputs
like groups and repeatable groups.

The first type parameter `id` corresponds to an optional identifier, used for
referencing a specific [field](FormToolkit.Decode#field) while decoding or
updating attributes, the type parameter `val` refers to a [value of an
arbitrary type](FormToolkit.Value#custom) which can be assigned to a certain
value of a [select](#select).

Not always do you care about decoding the form or having selects with custom type
values in which case you can annotate the type of your `Input` as
`Input id val`, or however you prefer.
It is possible to use a string or any other type to identify inputs, but this
would forego type safety.

-}
type alias Input id val =
    Internal.Input id val (Error id val)


{-| A message generated by a user input.
-}
type alias Msg id val =
    Internal.Msg id val


{-| Update a form passing a decoder to validate and produce a result,
and a [Msg](#Msg) to reflect user interactions.
-}
update :
    Decoder id val a
    -> Msg id val
    -> Input id val
    -> ( Input id val, Result (List (Error id val)) a )
update decoder msg input =
    Internal.update msg input
        |> FormToolkit.Decode.validateAndDecode decoder


{-|

    view : Html (Never -> a)
    view =
        Input.group []
            [ Input.text [ Input.label "First Name" ]
            , Input.text [ Input.label "Last Name" ]
            ]
            |> Input.toHtml (always never)

-}
toHtml : (Msg id val -> msg) -> Input id val -> Html msg
toHtml onChange input =
    View.init
        { events =
            { onChange = \path val -> onChange (InputChanged path val)
            , onFocus = onChange << InputFocused
            , onBlur = onChange << InputBlured
            , onAdd = onChange << InputsAdded
            , onRemove = onChange << InputsRemoved
            }
        , path = []
        , input = input
        }
        |> View.toHtml


{-| Text input field.

    usernameInput : Input id val
    usernameInput =
        text [ label "Username", placeholder "Enter your username" ]

-}
text : List (Attribute id val) -> Input id val
text =
    init Internal.Text


{-| Textarea input field.

    commentsInput : Input id val
    commentsInput =
        textarea [ label "Comments", placeholder "Enter your comments here" ]

-}
textarea : List (Attribute id val) -> Input id val
textarea =
    init Internal.TextArea


{-| Email input field.

    emailInput : Input id val
    emailInput =
        email [ label "Email", required True ]

-}
email : List (Attribute id val) -> Input id val
email =
    init Internal.Email


{-| Password input field.

    passwordInput : Input id val
    passwordInput =
        password [ label "Password", required True ]

-}
password : List (Attribute id val) -> Input id val
password =
    init Internal.Password


{-| Text input with strict autocomplete, if input doesn't match a
provided option the input value will be blank, value can be of any kind.

For non strict autocomplete use [text](#text) providing [options](#options)
attribute.
For string value options see [stringOptions](#stringOptions).

    type Lang
        = ES
        | EN
        | DE

    languageInput : Input id Lang
    languageInput =
        strictAutocomplete
            [ label "Language"
            , options
                [ ( "Español", Value.custom ES )
                , ( "English", Value.custom EN )
                , ( "Deutsch", Value.custom DE )
                ]
            ]

-}
strictAutocomplete : List (Attribute id val) -> Input id val
strictAutocomplete =
    init Internal.StrictAutocomplete


{-| Integer input field.

    ageInput : Input id val
    ageInput =
        int [ label "Age", min (Value.int 0), max (Value.int 120) ]

-}
int : List (Attribute id val) -> Input id val
int =
    init Internal.Integer


{-| Floating-point number input field.

    priceInput : Input id val
    priceInput =
        float [ label "Price", min (Value.float 0.0) ]

-}
float : List (Attribute id val) -> Input id val
float =
    init Internal.Float


{-| Date input field.

    birthdateInput : Input id val
    birthdateInput =
        date [ label "Birthdate", required True ]

-}
date : List (Attribute id val) -> Input id val
date =
    init Internal.Date


{-| Month input field.

    monthInput : Input id val
    monthInput =
        month [ label "Expiry Month" ]

-}
month : List (Attribute id val) -> Input id val
month =
    init Internal.Month


{-| Select input field (dropdown).

    type Lang
        = ES
        | EN
        | DE

    langSelect : Input id Lang
    langSelect =
        select
            [ label "Language"
            , options
                [ ( "Español", Value.custom ES )
                , ( "English", Value.custom EN )
                , ( "Deutsch", Value.custom DE )
                ]
            ]

-}
select : List (Attribute id val) -> Input id val
select =
    init Internal.Select


{-| Radio button input field.

    lightOnInput : Input id val
    lightOnInput =
        radio
            [ label "Light is"
            , options
                [ ( "On", Value.bool True )
                , ( "Off", Value.bool False )
                ]
            ]

-}
radio : List (Attribute id val) -> Input id val
radio =
    init Internal.Radio


{-| Checkbox input field.

    consentInput : Input id val
    consentInput =
        checkbox [ label "Subscribe to newsletter" ]

-}
checkbox : List (Attribute id val) -> Input id val
checkbox =
    init Internal.Checkbox


{-| Makes a group of inputs from a list of inputs.

Groups multiple inputs together.

    nameInputs : Input id val
    nameInputs =
        group []
            [ text [ name "firstName", label "First Name" ]
            , text [ name "lastName", label "Last Name" ]
            ]

-}
group : List (Attribute id val) -> List (Input id val) -> Input id val
group attributes =
    Tree.branch (Internal.init Internal.Group (unwrapAttrs attributes))


{-| Makes a repeatable group of inputs, from a template input or group and a
list of populated inputs.
The markup for the repeatable group will include buttons for adding,
and removing the repeatable input or group, by using `repeatableMin` and
`repeatableMax` it's possible to specify a lower and upper limit of the number
of instances that can be added or removed.

Allows inputs to be repeated multiple times.

    emailsInputs : Input id val
    emailsInputs =
        repeatable
            [ name "emails"
            , repeatableMin 1
            , repeatableMax 5
            , copies { addInputButton = "Add email address", removeInputButton = "Remove" }
            ]
            (text [ placeholder "Enter email address" ])
            [ text
                [ placeholder "Enter email address"
                , value (Value.string "mail@example.com")
                ]
            ]

-}
repeatable : List (Attribute id val) -> Input id val -> List (Input id val) -> Input id val
repeatable attributes template inputs =
    let
        params =
            Internal.init (Internal.Repeatable template)
                (unwrapAttrs attributes)

        children =
            inputs
                ++ List.repeat
                    (params.repeatableMin - List.length inputs)
                    template
    in
    Tree.branch params children


init : Internal.InputType id val (Error id val) -> List (Attribute id val) -> Input id val
init inputType attributes =
    Tree.leaf (Internal.init inputType (unwrapAttrs attributes))


unwrapAttrs :
    List (Attribute id val)
    -> List (Attributes id val (Error id val) -> Attributes id val (Error id val))
unwrapAttrs =
    List.map (\(Attribute f) -> f)


{-| Represents an attribute that can be applied to an input.
-}
type Attribute id val
    = Attribute (Attributes id val (Error id val) -> Attributes id val (Error id val))


{-| Sets the name of an input.

        -- import FormToolkit.Decode exposing (decode, json)

        text
            [ label "First name"
            , name "first-name"
            , value (Value.string "Chavela")
            ]
            |> decode json
            |> Result.map (Json.Encode.encode 0)
            -- Ok "{\"first-name\":\"Chavela\"}"

-}
name : String -> Attribute id val
name str =
    Attribute (\input -> { input | name = Just str })


{-| Sets the identifier to be reference when decoding a specific field. Used
also to extract a [partial view](FormToolkit.View#partial) or reference a field
in an [Error](FormToolkit.Decode#Error).

    import FormToolkit.Decode exposing (decode, field, string)

    type Fields
        = FirstName
        | LastName

    form : Input Fields value
    form =
        group []
            [ text
                [ label "First name"
                , identifier FirstName
                , value (Value.string "Juan")
                ]
            , text
                [ label "Last name"
                , identifier LastName
                , value (Value.string "Perez")
                ]
            ]

    decoded =
        form |> decode (field FirstName string)

    -- Ok "Juan"

-}
identifier : id -> Attribute id val
identifier id =
    Attribute (\input -> { input | identifier = Just id })


{-| Sets the value of an input. See [Value](FormToolkit.Value#Value)

    -- import FormToolkit.Decode exposing (decode, string)
    text [ label "Name", value (Value.string "Chavela") ]
        |> decode string
        == Ok "Chavela"

-}
value : Value.Value val -> Attribute id val
value (Value.Value inputValue) =
    Attribute (\input -> { input | value = inputValue })


{-| Marks an input as required, parsing and validation will fail and the missing
field error will be displayed.

    -- import FormToolkit.Decode exposing (decode, maybe, string)
    text [ label "First name" ]
        |> decode (maybe string)
        == Ok Nothing

    text [ label "First name", identifier "name", required True ]
        |> decode (maybe string)
        == Err (IsBlank "name")

-}
required : Bool -> Attribute id val
required bool =
    Attribute (\input -> { input | isRequired = bool })


{-| Sets the text to be rendered as the label for a field, or for the legend for
a group or repeatable inputs group.
-}
label : String -> Attribute id val
label str =
    Attribute (\input -> { input | label = Just str })


{-| Sets the placeholder text of an input.
-}
placeholder : String -> Attribute id val
placeholder str =
    Attribute (\input -> { input | placeholder = Just str })


{-| Sets a hint or help text for an input.
-}
hint : String -> Attribute id val
hint str =
    Attribute (\input -> { input | hint = Just str })


{-| Sets the options for a select or radio, or datalist for a
string input autocomplete.

    yesSelect : Input id ( Bool, Bool )
    yesSelect =
        select
            [ label "Language"
            , value (Value.custom ( True, True ))
            , options
                [ ( "Yes-yes", Value.custom ( True, True ) )
                , ( "Yes-no", Value.custom ( True, False ) )
                , ( "No-yes", Value.custom ( False, True ) )
                , ( "No-no", Value.custom ( False, False ) )
                ]
            ]

-}
options : List ( String, Value.Value val ) -> Attribute id val
options values =
    Attribute
        (\input ->
            { input
                | options =
                    List.map (Tuple.mapSecond (\(Value.Value val) -> val))
                        values
            }
        )


{-| Sets string options for a select or radio, or datalist for a
string input autocomplete.

    flavourInput : Input id ( Bool, Bool )
    flavourInput =
        text
            [ label "Favorite favour"
            , stringOptions [ "Chocolate", "Pistaccio", "Caramel salt" ]
            ]

-}
stringOptions : List String -> Attribute id val
stringOptions values =
    options
        (List.map (\strVal -> ( strVal, Value.string strVal )) values)


{-| Sets the minimum value for an input when the value is scalar (int, float,
date, ...).
-}
min : Value.Value val -> Attribute id val
min (Value.Value val) =
    Attribute (\input -> { input | min = val })


{-| Sets the maximum value for an input when the value is scalar (int, float,
date, ...).
-}
max : Value.Value val -> Attribute id val
max (Value.Value val) =
    Attribute (\input -> { input | max = val })


{-| An attribute that does nothing.
-}
noattr : Attribute id val
noattr =
    Attribute identity


{-| Sets the text for the add and remove buttons in a repeatable input.
-}
copies : { addInputButton : String, removeInputButton : String } -> Attribute id val
copies { addInputButton, removeInputButton } =
    Attribute
        (\input ->
            { input
                | addInputsButtonCopy = addInputButton
                , removeInputsButtonCopy = removeInputButton
            }
        )


{-| Sets the minimum number of copies for a repeatable input.
-}
repeatableMin : Int -> Attribute id val
repeatableMin integer =
    Attribute (\input -> { input | repeatableMin = integer })


{-| Sets the maximum number of copies for a repeatable input.
-}
repeatableMax : Int -> Attribute id val
repeatableMax integer =
    Attribute (\input -> { input | repeatableMax = Just integer })


{-| Update the attributes of an input passing an identifier and a list of
Attribute, it will fail if the input is not found.

    Input.group []
        [ Input.text
            [ Input.identifier "Input"
            , Input.value (Value.string "Value")
            ]
        ]
        |> Input.updateAttributes "Input"
            [ Input.value (Value.string "Updated") ]
        |> Result.mapError List.singleton
        |> Result.andThen
            (Decode.decode (Decode.field "Input" Decode.string))
        == Ok "Updated"

-}
updateAttributes : id -> List (Attribute id val) -> Input id val -> Result (Error id val) (Input id val)
updateAttributes id attrList input =
    if Tree.any (Internal.identifier >> (==) (Just id)) input then
        let
            attrs =
                unwrapAttrs attrList
        in
        Ok
            (Tree.map
                (\node ->
                    if Internal.identifier node == Just id then
                        Internal.updateAttributes attrs node

                    else
                        node
                )
                input
            )

    else
        Err (InputNotFound id)


{-| Collects all errors from an input and its children.
-}
errors : Input id val -> List (Error id val)
errors =
    Internal.errors


{-| Transforms identifiers or errors in an input, useful to combine inputs with
identifier of different type.

    type PersonFields
        = PersonName
        | PersonAge

    type TeamFields
        = TeamName
        | TeamMembers
        | MemberFields PersonFields

    personsFields : Input PersonFields
    personsFields =
        group []
            [ text
                [ label "Member Name"
                , identifier PersonName
                ]
            , int
                [ label "Member Age"
                , identifier PersonAge
                ]
            ]

    teamFields : Input TeamFields
    teamFields =
        group []
            [ text
                [ label "Team Name"
                , identifier TeamName
                ]
            , repeatable
                [ identifier TeamMembers ]
                -- ↓
                (map MemberFields personFields)
                -- ↑
                []
            ]

-}
map : (a -> b) -> Input a val -> Input b val
map func =
    Tree.mapValues (Internal.map func identity (mapError func identity))


mapError : (a -> b) -> (Value.Value val1 -> Value.Value val2) -> Error a val1 -> Error b val2
mapError transformId transformVal error =
    case error of
        ValueTooLarge id params ->
            ValueTooLarge (Maybe.map transformId id)
                { value = transformVal params.value
                , max = transformVal params.max
                }

        ValueTooSmall id params ->
            ValueTooSmall (Maybe.map transformId id)
                { value = transformVal params.value
                , min = transformVal params.min
                }

        ValueNotInRange id params ->
            ValueNotInRange (Maybe.map transformId id)
                { value = transformVal params.value
                , min = transformVal params.min
                , max = transformVal params.max
                }

        IsGroupNotInput id ->
            IsGroupNotInput (Maybe.map transformId id)

        IsBlank id ->
            IsBlank (Maybe.map transformId id)

        CustomError id err ->
            CustomError (Maybe.map transformId id) err

        ListError id params ->
            ListError (Maybe.map transformId id)
                { index = params.index
                , error = mapError transformId transformVal params.error
                }

        RepeatableHasNoName id ->
            RepeatableHasNoName (Maybe.map transformId id)

        InputNotFound id ->
            InputNotFound (transformId id)

        NoOptionsProvided id ->
            NoOptionsProvided (Maybe.map transformId id)

        ParseError id ->
            ParseError (Maybe.map transformId id)


{-| Map all of the values of an input, similary to [map](#map) that allows
combining inputs of different identifier type `mapValues` allows composing
inputs of different value type.

    select
        [ label "Language"
        , value (Value.custom ( True, False ))
        , options
            [ ( "Yes-yes", Value.custom ( True, True ) )
            , ( "Yes-no", Value.custom ( True, False ) )
            , ( "No-yes", Value.custom ( False, True ) )
            , ( "No-no", Value.custom ( False, False ) )
            ]
        ]
        |> mapValues (Value.mapCustom (Tuple.mapFirst not))
        == select
            [ label "Language"
            , value (Value.custom ( False, False ))
            , options
                [ ( "Yes-yes", Value.custom ( False, True ) )
                , ( "Yes-no", Value.custom ( False, False ) )
                , ( "No-yes", Value.custom ( True, True ) )
                , ( "No-no", Value.custom ( True, False ) )
                ]
            ]

-}
mapValues : (Value.Value val1 -> Value.Value val2) -> Input id val1 -> Input id val2
mapValues func =
    Tree.mapValues
        (Internal.map identity
            (\val ->
                case func (Value.Value val) of
                    Value.Value val_ ->
                        val_
            )
            (mapError identity func)
        )
