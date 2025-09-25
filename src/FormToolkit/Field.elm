module FormToolkit.Field exposing
    ( Field(..), Msg(..), update, toHtml
    , text, textarea, email, password, strictAutocomplete
    , int, float
    , date, month
    , select, radio, checkbox
    , group, repeatable
    , Attribute
    , name, identifier, value, required, label, placeholder, hint
    , options, stringOptions, min, max, autogrow
    , class, classList
    , noattr
    , copies, repeatableMin, repeatableMax
    , updateWithId, updateAttribute, updateAttributes
    , errors
    , map
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
@docs class, classList
@docs noattr


# Groups

@docs copies, repeatableMin, repeatableMax


# Update attributes

@docs updateWithId, updateAttribute, updateAttributes


# Error

@docs errors


# Mapping and composition

@docs map

-}

import FormToolkit.Error exposing (Error(..))
import FormToolkit.Value as Value
import Html exposing (Html)
import Internal.Field
    exposing
        ( Attributes
        , Field
        , FieldType
        , Msg(..)
        )
import Internal.Parse
import Internal.View
import RoseTree.Tree as Tree
import String.Extra


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
type Field id
    = Field (Internal.Field.Field id (Error id))


{-| A message generated through interaction with an input.
-}
type Msg id
    = Msg (Internal.Field.Msg id)


{-| Updates a form by passing a decoder to validate and produce a result,
and a [Msg](#Msg) to reflect user interactions.
-}
update : Msg id -> Field id -> Field id
update (Msg msg) (Field field) =
    Field
        (Internal.Field.update msg field
            |> Internal.Parse.validate
        )


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
toHtml : (Msg id -> msg) -> Field id -> Html msg
toHtml onChange (Field field) =
    Internal.View.init
        { events =
            { onChange = \path val -> onChange (Msg (InputChanged path val))
            , onFocus = onChange << Msg << InputFocused
            , onBlur = onChange << Msg << InputBlured
            , onAdd = onChange << Msg << InputsAdded
            , onRemove = onChange << Msg << InputsRemoved
            }
        , path = []
        , field = field
        }
        |> Internal.View.toHtml


{-| Builds a text input field.

[options](#options) can be provided to construct a `datalist` for autocomplete
suggestions.

    usernameField : Field id
    usernameField =
        text
            [ label "Username"
            , placeholder "Enter your username"
            ]

-}
text : List (Attribute id val) -> Field id
text =
    init Internal.Field.Text


{-| Builds a Textarea input field.

    commentsField : Field id
    commentsField =
        textarea
            [ label "Comments"
            , autogrow True
            , placeholder "Enter your comments here"
            ]

-}
textarea : List (Attribute id val) -> Field id
textarea =
    init Internal.Field.TextArea


{-| Builds an email input field.

    emailField : Field id
    emailField =
        email [ label "Email", required True ]

-}
email : List (Attribute id val) -> Field id
email =
    init Internal.Field.Email


{-| Builds a password input field.

    passwordField : Field id
    passwordField =
        password [ label "Password", required True ]

-}
password : List (Attribute id val) -> Field id
password =
    init Internal.Field.Password


{-| Builds a text input field with strict autocomplete. If the user input doesn't
match a provided option, the input value will be blank. The value can be of
any type.

For non-strict autocomplete, use [text](#text) with the [options](#options)
attribute. For string value options, see [stringOptions](#stringOptions).

    strictAutocomplete
        [ label "Language"
        , stringOptions [ "Español", "English", "Deutsch" ]
        ]

-}
strictAutocomplete : List (Attribute id val) -> Field id
strictAutocomplete =
    init Internal.Field.StrictAutocomplete


{-| Builds an integer input field.

    ageField : Field id
    ageField =
        int
            [ label "Age"
            , min (Value.int 0)
            , max (Value.int 120)
            ]

-}
int : List (Attribute id val) -> Field id
int =
    init Internal.Field.Integer


{-| Builds a floating-point number input field.

    priceField : Field id
    priceField =
        float [ label "Price", min (Value.float 0.0) ]

-}
float : List (Attribute id val) -> Field id
float =
    init Internal.Field.Float


{-| Builds a date input field.

    birthdateField : Field id
    birthdateField =
        date [ label "Birthdate", required True ]

-}
date : List (Attribute id val) -> Field id
date =
    init Internal.Field.Date


{-| Builds a month input field.

    monthField : Field id
    monthField =
        month [ label "Expiry Month" ]

-}
month : List (Attribute id val) -> Field id
month =
    init Internal.Field.Month


{-| Builds a select input field (dropdown).

    langSelect : Field id
    langSelect =
        select
            [ label "Language"
            , stringOptions [ "Español", "English", "Deutsch" ]
            ]

-}
select : List (Attribute id val) -> Field id
select =
    init Internal.Field.Select


{-| Builds a radio button input field.

    lightOnField : Field id
    lightOnField =
        radio
            [ label "Light is"
            , options
                [ ( "On", Value.bool True )
                , ( "Off", Value.bool False )
                ]
            ]

-}
radio : List (Attribute id val) -> Field id
radio =
    init Internal.Field.Radio


{-| Builds a checkbox input field.

    consentField : Field id
    consentField =
        checkbox [ label "Subscribe to newsletter" ]

-}
checkbox : List (Attribute id val) -> Field id
checkbox =
    init Internal.Field.Checkbox


{-| Groups a list of fields.

    nameFields : Field id
    nameFields =
        group
            [ name "person-name" ]
            [ text [ name "firstName", label "First Name" ]
            , text [ name "lastName", label "Last Name" ]
            ]

-}
group : List (Attribute id val) -> List (Field id) -> Field id
group attributes =
    List.map (\(Field field) -> field)
        >> Tree.branch
            (Internal.Field.init Internal.Field.Group
                (unwrapAttrs attributes)
            )
        >> Field


{-| Builds a repeatable group of fields from a template field.

A list of field update functions can be passed as the second argument to apply
to the template, creating one field per function.

The markup includes buttons for adding and removing fields.

Relevant attributes are [repeatableMin](#repeatableMin),
[repeatableMax](#repeatableMax), and [copies](#copies).

    import FormToolkit.Parse as Parse
    import FormToolkit.Value as Value

    emailsFields : Field id
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

    emailsFields
        |> Parse.parse (Parse.list Parse.string)
        |> Parse.toResult
    --> Ok [ "email@example.com", "other-email@example.com" ]

-}
repeatable :
    List (Attribute id val)
    -> Field id
    -> List (Field id -> Field id)
    -> Field id
repeatable attributes (Field template) updates =
    let
        params =
            Internal.Field.init (Internal.Field.Repeatable template)
                (unwrapAttrs attributes)

        children =
            List.concat
                [ updates
                    |> List.map
                        (\fn ->
                            let
                                (Field fields) =
                                    fn (Field template)
                            in
                            fields
                        )
                , List.repeat
                    (params.repeatableMin - List.length updates)
                    template
                ]
    in
    Field (Tree.branch params children)


init : FieldType id (Error id) -> List (Attribute id val) -> Field id
init inputType attributes =
    let
        field =
            Tree.leaf (Internal.Field.init inputType (unwrapAttrs attributes))

        fieldWithMissingOptions =
            Internal.Field.setErrors
                [ NoOptionsProvided (Internal.Field.identifier field)
                ]
                field
    in
    case ( Internal.Field.inputType field, Internal.Field.options field ) of
        ( Internal.Field.Select, [] ) ->
            Field fieldWithMissingOptions

        ( Internal.Field.Radio, [] ) ->
            Field fieldWithMissingOptions

        ( Internal.Field.StrictAutocomplete, [] ) ->
            Field fieldWithMissingOptions

        _ ->
            Field field


unwrapAttrs :
    List (Attribute id val)
    -> List (Attributes id (Error id) -> Attributes id (Error id))
unwrapAttrs =
    List.map (\(Attribute f) -> f)


{-| Represents an attribute that can be applied to a field.
-}
type Attribute id val
    = Attribute (Attributes id (Error id) -> Attributes id (Error id))


{-| Sets the name of a field.

        import FormToolkit.Parse as Parse
        import Json.Encode

        text
            [ label "First name"
            , name "first-name"
            , value (Value.string "Chavela")
            ]
            |> Parse.parse Parse.json
            |> Parse.toResult
            |> Result.map (Json.Encode.encode 0)
            --> Ok "{\"first-name\":\"Chavela\"}"

-}
name : String -> Attribute id val
name str =
    combineAttrs
        -- NOT TESTED
        (class (dasherize str))
        (Attribute (\field -> { field | name = Just (dasherize str) }))


{-| Sets the identifier to be referenced when decoding a specific field,
extracting a segment of the form, updating the field's attributes, or
customizing the rendering of a specific field or field error.

Any type can be used as an identifier, but using a custom type is encouraged
for added type safety.

    import FormToolkit.Parse as Parse
    import FormToolkit.Value as Value

    type Fields
        = FirstName
        | LastName

    form : Field Fields
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
        |> Parse.parse
            (Parse.field FirstName Parse.string)
        |> Parse.toResult
        --> Ok "Juan"

-}
identifier : id -> Attribute id val
identifier id =
    Attribute (\field -> { field | identifier = Just id })


{-| Sets the value of a field.

    import FormToolkit.Parse as Parse
    import FormToolkit.Value as Value

    text [ label "Name", value (Value.string "Chavela") ]
        |> Parse.parse Parse.string
        |> Parse.toResult
        --> Ok "Chavela"

-}
value : Value.Value -> Attribute id val
value (Value.Value inputValue) =
    Attribute (\field -> { field | value = inputValue })


{-| Marks a field as required, parsing and validation will fail and the missing
field error will be displayed.

    import FormToolkit.Parse as Parse
    import FormToolkit.Value as Value

    text [ label "First name" ]
        |> Parse.parse (Parse.maybe Parse.string)
        |> Parse.toResult
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
    combineAttrs
        -- NOT TESTED
        (class (dasherize str))
        (Attribute (\field -> { field | label = Just str }))


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

    select
        [ label "Agreed?"
        , options [ ( "Yes", Value.bool True ), ( "No", Value.bool False ) ]
        ]

-}
options : List ( String, Value.Value ) -> Attribute id val
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

    flavorField : Field id ( Bool, Bool )
    flavorField =
        text
            [ label "Favorite flavor"
            , stringOptions
                [ "Chocolate"
                , "Pistachio"
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
min : Value.Value -> Attribute id val
min (Value.Value val) =
    Attribute (\field -> { field | min = val })


{-| Sets the maximum value for a field input if its value is scalar:
`int`, `float`, `date`, `month`, or `time`.
-}
max : Value.Value -> Attribute id val
max (Value.Value val) =
    Attribute (\field -> { field | max = val })


{-| Makes a `textarea` autogrow.
-}
autogrow : Bool -> Attribute id val
autogrow shouldAutogrow =
    Attribute (\field -> { field | autogrow = shouldAutogrow })


{-| Apply a CSS class
-}
class : String -> Attribute id val
class str =
    Attribute
        (\field ->
            { field | classList = str :: field.classList }
        )


{-| Apply a conditional list of CSS classes
-}
classList : List ( String, Bool ) -> Attribute id val
classList =
    List.filter Tuple.second
        >> List.map (Tuple.first >> class)
        >> List.foldl combineAttrs noattr


{-| An attribute that does nothing.
-}
noattr : Attribute id val
noattr =
    Attribute identity


combineAttrs : Attribute id val -> Attribute id val -> Attribute id val
combineAttrs (Attribute a) (Attribute b) =
    Attribute (b >> a)


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


{-| Updates a field corresponding to [identifier](#identifier).

    import FormToolkit.Parse as Parse
    import FormToolkit.Value as Value

    group []
        [ text
            [ identifier "Field"
            , value (Value.string "Value")
            ]
        ]
        |> updateWithId "Field"
            (updateAttribute
                (value (Value.string "Updated"))
            )
        |> Result.andThen
            (\field ->
                field
                    |> Parse.parse (Parse.field "Field" Parse.string)
                    |> Parse.toResult
            )
        --> Ok "Updated"

-}
updateWithId : id -> (Field id -> Field id) -> Field id -> Result (List (Error id)) (Field id)
updateWithId id fn (Field field) =
    if
        Tree.any (\node -> Internal.Field.identifier node == Just id)
            field
    then
        Ok
            (field
                |> Tree.map
                    (\node ->
                        if Internal.Field.identifier node == Just id then
                            let
                                (Field updated) =
                                    fn (Field node)
                            in
                            updated

                        else
                            node
                    )
                |> Field
            )

    else
        Err [ InputNotFound id ]


{-| Updates a field attribute.

    import FormToolkit.Parse as Parse
    import FormToolkit.Value as Value

    text
        [ value (Value.string "Value") ]
        |> updateAttribute (value (Value.string "Updated"))
        |> Parse.parse Parse.string
        |> Parse.toResult
        --> Ok "Updated"

-}
updateAttribute : Attribute id val -> Field id -> Field id
updateAttribute attr =
    updateAttributes [ attr ]


{-| Updates several field attributes.

    import FormToolkit.Parse as Parse
    import FormToolkit.Value as Value

    text
        [ identifier "Field"
        , stringOptions [ "Vanilla", "Lemon", "Yogurt" ]
        ]
        |> updateAttributes
            [ value (Value.string "Chocolate")
            , stringOptions
                [ "Chocolate"
                , "Pistachio"
                , "Caramel salt"
                ]
            ]
        |> Parse.parse Parse.string
        |> Parse.toResult
        --> Ok "Chocolate"

-}
updateAttributes : List (Attribute id val) -> Field id -> Field id
updateAttributes attrList (Field field) =
    Field
        (Internal.Field.updateAttributes (unwrapAttrs attrList) field
            |> Internal.Parse.validate
        )


{-| Collects all errors from a field and its children.
-}
errors : Field id -> List (Error id)
errors (Field field) =
    Internal.Field.errors field


{-| Transforms identifiers or errors in a field, useful for combining fields
with identifiers of different types.

    type PersonFields
        = PersonName
        | PersonAge

    type TeamFields
        = TeamName
        | TeamMembers
        | MemberFields PersonFields

    personsFields : Field PersonFields
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

    teamFields : Field TeamFields
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
map : (a -> b) -> Field a -> Field b
map func (Field field) =
    Field (Tree.mapValues (Internal.Field.map func (mapError func)) field)


mapError : (a -> b) -> Error a -> Error b
mapError transformId error =
    case error of
        ValueTooLarge id params ->
            ValueTooLarge (Maybe.map transformId id)
                { value = params.value
                , max = params.max
                }

        ValueTooSmall id params ->
            ValueTooSmall (Maybe.map transformId id)
                { value = params.value
                , min = params.min
                }

        ValueNotInRange id params ->
            ValueNotInRange (Maybe.map transformId id)
                { value = params.value
                , min = params.min
                , max = params.max
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
                , error = mapError transformId params.error
                }

        RepeatableHasNoName id ->
            RepeatableHasNoName (Maybe.map transformId id)

        InputNotFound id ->
            InputNotFound (transformId id)

        NoOptionsProvided id ->
            NoOptionsProvided (Maybe.map transformId id)

        ParseError id ->
            ParseError (Maybe.map transformId id)


dasherize : String -> String
dasherize =
    String.Extra.decapitalize >> String.Extra.dasherize
