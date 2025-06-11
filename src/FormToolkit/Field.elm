module FormToolkit.Field exposing
    ( Field, Msg, update, toHtml
    , text, textarea, email, password, strictAutocomplete
    , int, float
    , date, month
    , select, radio, checkbox
    , group, repeatable
    , Attribute
    , name, identifier, value, required, label, placeholder, hint
    , options, stringOptions, min, max, autogrow
    , noattr
    , copies, repeatableMin, repeatableMax
    , updateBy, updateAttribute, updateAttributes
    , errors
    , map, mapValues
    )

{-| Provides types and functions to create form fields of various types, set
their attributes, update, and render them.


# Field

@docs Field, Msg, update, toHtml


# Field types

@docs text, textarea, email, password, strictAutocomplete
@docs int, float
@docs date, month
@docs select, radio, checkbox
@docs group, repeatable


# Attributes

@docs Attribute
@docs name, identifier, value, required, label, placeholder, hint
@docs options, stringOptions, min, max, autogrow
@docs noattr


# Groups

@docs copies, repeatableMin, repeatableMax


# Update attributes

@docs updateBy, updateAttribute, updateAttributes


# Error

@docs errors


# Mapping and composition

@docs map, mapValues

-}

import FormToolkit.Parse exposing (Error(..), Parser)
import FormToolkit.Value as Value
import Html exposing (Html)
import Internal.Field as Internal
    exposing
        ( Attributes
        , Field
        , FieldType
        , Msg(..)
        )
import Internal.View as View
import RoseTree.Tree as Tree


{-| Represents a form field element, which can be an individual field or a group
of fields.

This element may consist of individual input fields (such as text, email, or
password), groups of fields, or repeatable fields.

The type parameter `id` corresponds to an optional identifier used for
referencing a specific `field` while decoding or updating attributes, and the
type parameter `val` refers to a value of an arbitrary type.

Fields are preferably identified by a custom type but can also be identified by
a `String`.

-}
type alias Field id val =
    Internal.Field id val (Error id val)


{-| A message generated through interaction with an input.
-}
type alias Msg id val =
    Internal.Msg id val


{-| Updates a form by passing a decoder to validate and produce a result,
and a [Msg](#Msg) to reflect user interactions.
-}
update :
    Parser id val a
    -> Msg id val
    -> Field id val
    -> ( Field id val, Result (List (Error id val)) a )
update decoder msg field =
    Internal.update msg field
        |> FormToolkit.Parse.validateAndParse decoder


{-| Renders the form.

    type UpdateMsg
        = FieldsUpdated Msg

    view : Html UpdateMsg
    view =
        group []
            [ text [ label "First Name" ]
            , text [ label "Last Name" ]
            ]
            |> toHtml FieldsUpdated

-}
toHtml : (Msg id val -> msg) -> Field id val -> Html msg
toHtml onChange field =
    View.init
        { events =
            { onChange = \path val -> onChange (InputChanged path val)
            , onFocus = onChange << InputFocused
            , onBlur = onChange << InputBlured
            , onAdd = onChange << InputsAdded
            , onRemove = onChange << InputsRemoved
            }
        , path = []
        , field = field
        }
        |> View.toHtml


{-| Builds a text input field.

[options](#options) can be provided to construct a `datalist` for autocomplete
suggestions.

    usernameField : Field id val
    usernameField =
        text
            [ label "Username"
            , placeholder "Enter your username"
            ]

-}
text : List (Attribute id val) -> Field id val
text =
    init Internal.Text


{-| Builds a Textarea input field.

    commentsField : Field id val
    commentsField =
        textarea
            [ label "Comments"
            , autogrow True
            , placeholder "Enter your comments here"
            ]

-}
textarea : List (Attribute id val) -> Field id val
textarea =
    init Internal.TextArea


{-| Builds an email input field.

    emailField : Field id val
    emailField =
        email [ label "Email", required True ]

-}
email : List (Attribute id val) -> Field id val
email =
    init Internal.Email


{-| Builds a password input field.

    passwordField : Field id val
    passwordField =
        password [ label "Password", required True ]

-}
password : List (Attribute id val) -> Field id val
password =
    init Internal.Password


{-| Builds a text input field with strict autocomplete. If the input doesn't
match a provided option, the input value will be blank. The value can be of
any type.

For non-strict autocomplete, use [text](#text) with the [options](#options)
attribute. For string value options, see [stringOptions](#stringOptions).

    type Lang
        = ES
        | EN
        | DE

    languageField : Field id Lang
    languageField =
        strictAutocomplete
            [ label "Language"
            , options
                [ ( "Español", Value.custom ES )
                , ( "English", Value.custom EN )
                , ( "Deutsch", Value.custom DE )
                ]
            ]

-}
strictAutocomplete : List (Attribute id val) -> Field id val
strictAutocomplete =
    init Internal.StrictAutocomplete


{-| Builds an integer input field.

    ageField : Field id val
    ageField =
        int
            [ label "Age"
            , min (Value.int 0)
            , max (Value.int 120)
            ]

-}
int : List (Attribute id val) -> Field id val
int =
    init Internal.Integer


{-| Builds a floating-point number input field.

    priceField : Field id val
    priceField =
        float [ label "Price", min (Value.float 0.0) ]

-}
float : List (Attribute id val) -> Field id val
float =
    init Internal.Float


{-| Builds a date input field.

    birthdateField : Field id val
    birthdateField =
        date [ label "Birthdate", required True ]

-}
date : List (Attribute id val) -> Field id val
date =
    init Internal.Date


{-| Builds a month input field.

    monthField : Field id val
    monthField =
        month [ label "Expiry Month" ]

-}
month : List (Attribute id val) -> Field id val
month =
    init Internal.Month


{-| Builds a select input field (dropdown).

    type Lang
        = ES
        | EN
        | DE

    langSelect : Field id Lang
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
select : List (Attribute id val) -> Field id val
select =
    init Internal.Select


{-| Builds a radio button input field.

    lightOnField : Field id val
    lightOnField =
        radio
            [ label "Light is"
            , options
                [ ( "On", Value.bool True )
                , ( "Off", Value.bool False )
                ]
            ]

-}
radio : List (Attribute id val) -> Field id val
radio =
    init Internal.Radio


{-| Builds a checkbox input field.

    consentField : Field id val
    consentField =
        checkbox [ label "Subscribe to newsletter" ]

-}
checkbox : List (Attribute id val) -> Field id val
checkbox =
    init Internal.Checkbox


{-| Groups a list of fields.

    nameFields : Field id val
    nameFields =
        group
            [ name "person-name" ]
            [ text [ name "firstName", label "First Name" ]
            , text [ name "lastName", label "Last Name" ]
            ]

-}
group : List (Attribute id val) -> List (Field id val) -> Field id val
group attributes =
    Tree.branch (Internal.init Internal.Group (unwrapAttrs attributes))


{-| Builds a repeatable group of fields from a template field.

A list of field update functions can be passed as the second argument to apply
to the template, creating one field per function.

The markup includes buttons for adding and removing fields.

Relevant attributes are [repeatableMin](#repeatableMin),
[repeatableMax](#repeatableMax), and [copies](#copies).

    import FormToolkit.Decode as Decode
    import FormToolkit.Value as Value

    emailsFields : Field id val
    emailsFields =
        repeatable
            [ name "emails"
            , repeatableMin 1
            , repeatableMax 5
            , copies
                { addFieldsButton = "Add email address"
                , removeFieldsButton = "Remove"
                }
            ]
            (text [ placeholder "Enter email address" ])
            [ updateAttribute
                (value
                    (Value.string "email@example.com")
                )
            , updateAttribute
                (value
                    (Value.string "other-email@example.com")
                )
            ]

    emailsFields |> Decode.decode (Decode.list Decode.string)
    --> Ok [ "email@example.com", "other-email@example.com" ]

-}
repeatable :
    List (Attribute id val)
    -> Field id val
    -> List (Field id val -> Field id val)
    -> Field id val
repeatable attributes template updates =
    let
        params =
            Internal.init (Internal.Repeatable template) (unwrapAttrs attributes)

        children =
            List.concat
                [ List.map (\fn -> fn template) updates
                , List.repeat (params.repeatableMin - List.length updates) template
                ]
    in
    Tree.branch params children


init : FieldType id val (Error id val) -> List (Attribute id val) -> Field id val
init inputType attributes =
    Tree.leaf (Internal.init inputType (unwrapAttrs attributes))


unwrapAttrs :
    List (Attribute id val)
    -> List (Attributes id val (Error id val) -> Attributes id val (Error id val))
unwrapAttrs =
    List.map (\(Attribute f) -> f)


{-| Represents an attribute that can be applied to a field.
-}
type Attribute id val
    = Attribute (Attributes id val (Error id val) -> Attributes id val (Error id val))


{-| Sets the name of a field.

        import FormToolkit.Decode as Decode
        import Json.Encode

        text
            [ label "First name"
            , name "first-name"
            , value (Value.string "Chavela")
            ]
            |> Decode.decode Decode.json
            |> Result.map (Json.Encode.encode 0)
            --> Ok "{\"first-name\":\"Chavela\"}"

-}
name : String -> Attribute id val
name str =
    Attribute (\field -> { field | name = Just str })


{-| Sets the identifier to be referenced when decoding a specific field,
extracting a segment of the form, updating the field's attributes, or
customizing the rendering of a specific field or field error.

Any type can be used as an identifier, but using a custom type is encouraged
for added type safety.

    import FormToolkit.Decode as Decode
    import FormToolkit.Value as Value

    type Fields
        = FirstName
        | LastName

    form : Field Fields value
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

    form
        |> Decode.decode
            (Decode.field FirstName Decode.string)
        --> Ok "Juan"

-}
identifier : id -> Attribute id val
identifier id =
    Attribute (\field -> { field | identifier = Just id })


{-| Sets the value of a field.

    import FormToolkit.Decode as Decode
    import FormToolkit.Value as Value

    text [ label "Name", value (Value.string "Chavela") ]
        |> Decode.decode Decode.string
        --> Ok "Chavela"

-}
value : Value.Value val -> Attribute id val
value (Value.Value inputValue) =
    Attribute (\field -> { field | value = inputValue })


{-| Marks a field as required, parsing and validation will fail and the missing
field error will be displayed.

    import FormToolkit.Decode as Decode
    import FormToolkit.Value as Value

    text [ label "First name" ]
        |> Decode.decode (Decode.maybe Decode.string)
        --> Ok Nothing

-}
required : Bool -> Attribute id val
required bool =
    Attribute (\field -> { field | isRequired = bool })


{-| Sets the text to be rendered as the label for a field, or for the legend for
a group or repeatable fields group.
-}
label : String -> Attribute id val
label str =
    Attribute (\field -> { field | label = Just str })


{-| Sets the placeholder text of a field.
-}
placeholder : String -> Attribute id val
placeholder str =
    Attribute (\field -> { field | placeholder = Just str })


{-| Sets a hint or help text for a field.
-}
hint : String -> Attribute id val
hint str =
    Attribute (\field -> { field | hint = Just str })


{-| Sets the options for a [select](#select), [radio](#radio) button, or
`datalist` for a [text](#text) field or [strictAutocomplete](#strictAutocomplete)
to provide autocomplete suggestions.

    yesSelect : Field id ( Bool, Bool )
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
        (\field ->
            { field
                | options =
                    List.map (Tuple.mapSecond (\(Value.Value val) -> val))
                        values
            }
        )


{-| Sets string options for a [select](#select), [radio](#radio) button, or
`datalist` for a [text](#text) field or [strictAutocomplete](#strictAutocomplete)
to provide autocomplete suggestions.

    flavourField : Field id ( Bool, Bool )
    flavourField =
        text
            [ label "Favorite favour"
            , stringOptions
                [ "Chocolate"
                , "Pistaccio"
                , "Caramel salt"
                ]
            ]

-}
stringOptions : List String -> Attribute id val
stringOptions values =
    options (List.map (\strVal -> ( strVal, Value.string strVal )) values)


{-| Sets the minimum value for a field input if its value is scalar:
`int`, `float`, `date`, `month`, or `time`.
-}
min : Value.Value val -> Attribute id val
min (Value.Value val) =
    Attribute (\field -> { field | min = val })


{-| Sets the maximum value for a field input if its value is scalar:
`int`, `float`, `date`, `month`, or `time`.
-}
max : Value.Value val -> Attribute id val
max (Value.Value val) =
    Attribute (\field -> { field | max = val })


{-| Makes a `textarea` autogrow.
-}
autogrow : Bool -> Attribute id val
autogrow shouldAutogrow =
    Attribute (\field -> { field | autogrow = shouldAutogrow })


{-| An attribute that does nothing.
-}
noattr : Attribute id val
noattr =
    Attribute identity


{-| Sets the text for the add and remove buttons in a repeatable field.
-}
copies : { addFieldsButton : String, removeFieldsButton : String } -> Attribute id val
copies { addFieldsButton, removeFieldsButton } =
    Attribute
        (\field ->
            { field
                | addFieldsButtonCopy = addFieldsButton
                , removeFieldsButtonCopy = removeFieldsButton
            }
        )


{-| Sets the minimum number of instances for a repeating field for a
[repeatable](#repeatable).
-}
repeatableMin : Int -> Attribute id val
repeatableMin integer =
    Attribute (\field -> { field | repeatableMin = integer })


{-| Sets the maximum number of instances for a repeating field for a
[repeatable](#repeatable).
-}
repeatableMax : Int -> Attribute id val
repeatableMax integer =
    Attribute (\field -> { field | repeatableMax = Just integer })


{-| Finds a nested field by [identifier](#identifier) and, if the nested
field is found, returns the topmost field with the nested field updated by the
provided function.

    import FormToolkit.Decode as Decode
    import FormToolkit.Value as Value

    group []
        [ text
            [ identifier "Field"
            , value (Value.string "Value")
            ]
        ]
        |> updateBy "Field"
            (updateAttribute
                (value (Value.string "Updated"))
            )
        |> Maybe.map
            (Decode.decode
                (Decode.field "Field" Decode.string)
            )
        --> Just (Ok "Updated")

-}
updateBy : id -> (Field id val -> Field id val) -> Field id val -> Maybe (Field id val)
updateBy id fn field =
    if Tree.any (\node -> Internal.identifier node == Just id) field then
        Just
            (field
                |> Tree.map
                    (\node ->
                        if Internal.identifier node == Just id then
                            fn node

                        else
                            node
                    )
            )

    else
        Nothing


{-| Updates a field attribute.

    import FormToolkit.Decode as Decode
    import FormToolkit.Value as Value

    text
        [ value (Value.string "Value") ]
        |> updateAttribute (value (Value.string "Updated"))
        |> Decode.decode Decode.string
        --> Ok "Updated"

-}
updateAttribute : Attribute id val -> Field id val -> Field id val
updateAttribute attr =
    Internal.updateAttributes (unwrapAttrs [ attr ])


{-| Updates several field attributes.

    import FormToolkit.Decode as Decode
    import FormToolkit.Value as Value

    text
        [ identifier "Field"
        , stringOptions [ "Vanilla", "Lemon", "Yogurt" ]
        ]
        |> updateAttributes
            [ value (Value.string "Chocolate")
            , stringOptions
                [ "Chocolate"
                , "Pistaccio"
                , "Caramel salt"
                ]
            ]
        |> Decode.decode Decode.string
        --> Ok "Chocolate"

-}
updateAttributes : List (Attribute id val) -> Field id val -> Field id val
updateAttributes attrList =
    Internal.updateAttributes (unwrapAttrs attrList)


{-| Collects all errors from a field and its children.
-}
errors : Field id val -> List (Error id val)
errors =
    Internal.errors


{-| Transforms identifiers or errors in a field, useful for combining fields
with identifiers of different types.

    type PersonFields
        = PersonName
        | PersonAge

    type TeamFields
        = TeamName
        | TeamMembers
        | MemberFields PersonFields

    personsFields : Field PersonFields val
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

    teamFields : Field TeamFields val
    teamFields =
        group []
            [ text
                [ label "Team Name"
                , identifier TeamName
                ]
            , repeatable
                [ identifier TeamMembers ]
                -- ↓↓↓↓
                (map MemberFields personFields)
                -- ↑↑↑↑
                []
            ]

-}
map : (a -> b) -> Field a val -> Field b val
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


{-| Maps all of the values of a field. Similar to [map](#map), which allows
combining fields with different identifier types, `mapValues` enables composing
fields with different value types.

    import FormToolkit.Value as Value

    (select
        [ label "Language"
        , value (Value.custom ( 1, False ))
        , options
            [ ( "Yes-yes", Value.custom ( 1, True ) )
            , ( "Yes-no", Value.custom ( 1, False ) )
            , ( "No-yes", Value.custom ( 0, True ) )
            , ( "No-no", Value.custom ( 0, False ) )
            ]
        ]
        |> mapValues (Value.mapCustom (Tuple.mapFirst String.fromInt))
        )
        == select
            [ label "Language"
            , value (Value.custom ( "1", False ))
            , options
                [ ( "Yes-yes", Value.custom ( "1", True ) )
                , ( "Yes-no", Value.custom ( "1", False ) )
                , ( "No-yes", Value.custom ( "0", True ) )
                , ( "No-no", Value.custom ( "0", False ) )
                ]
            ]
            --> True

-}
mapValues : (Value.Value val1 -> Value.Value val2) -> Field id val1 -> Field id val2
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
