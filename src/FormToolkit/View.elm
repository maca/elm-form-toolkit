module FormToolkit.View exposing
    ( View, fromField, toHtml
    , partial
    , Attribute, class, classList, style
    , InputType(..)
    , customizeError, customizeField
    , customizeGroup, customizeRepeatableFields, customizeRepeatingFieldTemplate
    )

{-|


# View

@docs View, fromField, toHtml
@docs partial


# View customizations


## Attributes

@docs Attribute, class, classList, style


## Markup customization

@docs InputType
@docs customizeError, customizeField
@docs customizeGroup, customizeRepeatableFields, customizeRepeatingFieldTemplate

-}

import FormToolkit.Decode exposing (Error)
import FormToolkit.Field exposing (Field)
import FormToolkit.Value exposing (Value(..))
import Html exposing (Html)
import Internal.Field exposing (Msg(..))
import Internal.View
import RoseTree.Tree as Tree


{-| A view is a way to configure the generated markdown for an `Field` or group
of fields.
-}
type View id val msg
    = View (Internal.View.View id val msg)


{-| Construct a view from an `Field`.

    view : Html (Never -> a)
    view =
        Field.group []
            [ Field.text [ Field.label "First Name" ]
            , Field.text [ Field.label "Last Name" ]
            ]
            |> View.fromField (always never)
            |> View.toHtml

-}
fromField : (Msg id val -> msg) -> Field id val -> View id val msg
fromField onChange field =
    View
        (Internal.View.init
            { events =
                { onChange = \path value -> onChange (InputChanged path value)
                , onFocus = onChange << InputFocused
                , onBlur = onChange << InputBlured
                , onAdd = onChange << InputsAdded
                , onRemove = onChange << InputsRemoved
                }
            , path = []
            , field = field
            }
        )


{-| Render a view
-}
toHtml : View id val msg -> Html msg
toHtml (View view) =
    Internal.View.toHtml view


{-| A partial view referenced by `identifier`.
Maybe you want to render segments of the same form in different UI sections.

    Field.group []
        [ Field.text
            [ Field.identifier "FirstName"
            , Field.label "First Name"
            ]
        , Field.text
            [ Field.identifier "LastName"
            , Field.label "Last Name"
            ]
        ]
        |> View.fromField (always never)
        |> View.partial "FirstName"
        == Just
            (Field.text
                [ Field.identifier "FirstName"
                , Field.label "First Name"
                ]
            )

-}
partial : id -> View id val msg -> Maybe (View id val msg)
partial id (View view) =
    Internal.View.partial id view |> Maybe.map View


{-| Customizes how the error copies are displayed, to be used for i18n errors.
It's possible to override a specific error message for all fields, an individual
field, or for fields of a certain type.

    type Fields
        = Name
        | Temperature
        | Flavour

    view : View Fields val ()
    view =
        Field.group []
            [ Field.text
                [ Field.label "Name"
                , Field.identifier Name
                , Field.required
                ]
            , Field.text
                [ Field.label "Temperature"
                , Field.identifier Temperature
                , Field.min 20
                , Field.max 35
                , Field.required
                ]
            , Field.select
                [ Field.label "Flavour"
                , Field.identifier Flavour
                , Field.required
                , Field.options
                    [ ( "Banana", Value.string "banana" )
                    , ( "Strawbery", Value.string "strawberry" )
                    , ( "Chocolate", Value.string "chocolate" )
                    ]
                ]
            ]
            |> View.fromField (always ())
            |> View.customizeError
                (\{ inputType, error } ->
                    let
                        toString =
                            Value.toString >> Maybe.withDefault ""
                    in
                    case ( inputType, error ) of
                        ( _, ValueTooLarge _ data ) ->
                            toString data.max ++ " is too high"

                        ( _, ValueTooSmall _ data ) ->
                            toString data.min ++ " is too low"

                        ( _, ValueNotInRange _ data ) ->
                            "Make it in between " ++ toString data.min ++ " and " ++ toString data.max

                        ( Select, IsBlank _ ) ->
                            "Make up your mind"

                        ( _, IsBlank (Just Name) ) ->
                            "Who are you?"

                        ( _, IsBlank _ ) ->
                            "You forgot to fill in this"

                        ( _, CustomError message ) ->
                            message

                        _ ->
                            "Humm...?"
                )

-}
customizeError :
    ({ inputType : InputType, error : Error id val } -> String)
    -> View id val msg
    -> View id val msg
customizeError viewFunc (View ({ attributes, field } as view)) =
    View
        { view
            | attributes =
                { attributes
                    | errorToString =
                        \error ->
                            let
                                unwrappedField =
                                    Tree.value field
                            in
                            viewFunc
                                { inputType = fieldTypeToInputType unwrappedField.inputType
                                , error = error
                                }
                }
        }


{-| Provide a function to override the rendering of a field.

`label` and `input` are functions that take a list of
[Attribute](#Attribute)s.

Use `advanced` parameters for a greater level of customization of the field.
It is possible to target specific fields by `InputType`, or `identifier`.

The example bellow would render the input exactly as it normaly renders :P

    view : View id val ()
    view =
        Field.text [ Field.label "Name" ]
            |> View.fromField (always ())
            |> customizeField
                (\{ isRequired, label, input, errors, hint } ->
                    Html.div
                        [ Attributes.class "field"
                        , Attributes.classList [ ( "required", isRequired ) ]
                        ]
                        [ -- ↓ call with ↓ Attribute list
                          label [ class "input-label" ]
                        , Html.div
                            [ Attributes.class "input-wrapper" ]
                            [ -- ↓ same here, label `for` already references the input
                              input []
                            ]
                        , case errors of
                            err :: _ ->
                                Html.p [ Attributes.class "errors" ] [ Html.text err ]

                            [] ->
                                hint []
                        ]
                )

-}
customizeField :
    ({ isRequired : Bool
     , label : List (Attribute msg) -> Html msg
     , input : List (Attribute msg) -> Html msg
     , hint : List (Attribute msg) -> Html msg
     , errors : List String
     , advanced :
        { identifier : Maybe id
        , inputType : InputType
        , inputName : Maybe String
        , inputPlaceholder : Maybe String
        , inputValue : Value val
        , inputMin : Value val
        , inputMax : Value val
        , inputOptions : List ( String, Value val )
        , inputOnChange : Value val -> msg
        , inputOnBlur : msg
        , inputOnFocus : msg
        , labelText : Maybe String
        , hintText : Maybe String
        , idString : String
        }
     }
     -> Html msg
    )
    -> View id val msg
    -> View id val msg
customizeField viewFunc (View ({ attributes, field } as view)) =
    View
        { view
            | attributes =
                { attributes
                    | fieldView =
                        \params ->
                            let
                                unwrappedField =
                                    Tree.value field
                            in
                            viewFunc
                                { isRequired = params.isRequired
                                , label = toAttrs >> params.label
                                , input = toAttrs >> params.input
                                , hint = toAttrs >> params.input
                                , errors = []
                                , advanced =
                                    { identifier = unwrappedField.identifier
                                    , inputName = unwrappedField.name
                                    , inputType = fieldTypeToInputType unwrappedField.inputType
                                    , inputPlaceholder = unwrappedField.placeholder
                                    , inputValue = Value unwrappedField.value
                                    , inputMin = Value unwrappedField.min
                                    , inputMax = Value unwrappedField.max
                                    , inputOptions =
                                        List.map (Tuple.mapSecond Value) unwrappedField.options
                                    , inputOnChange =
                                        \(Value val) -> attributes.onChange params.path val
                                    , inputOnBlur = attributes.onBlur params.path
                                    , inputOnFocus = attributes.onFocus params.path
                                    , labelText = unwrappedField.label
                                    , hintText = unwrappedField.hint
                                    , idString = Internal.View.inputId field params.path
                                    }
                                }
                }
        }


{-| Provide a function to customize the rendering of a group of fields.

    view : View id val ()
    view =
        Field.group []
            [ Field.text [ Field.label "Name" ]
            , Field.text [ Field.label "Last Name" ]
            ]
            |> View.fromField (always ())
            |> customizeField
                (\{ fields, legendText } ->
                    Html.fieldset
                        [ Html.Attribute.class "field-group" ]
                        ((case legendText of
                            Just str ->
                                Html.legend [] [ Html.text str ]

                            Nothing ->
                                Html.text ""
                         )
                            :: fields
                        )
                )

-}
customizeGroup :
    ({ legendText : Maybe String
     , fields : List (Html msg)
     , identifier : Maybe id
     , errors : List String
     }
     -> Html msg
    )
    -> View id val msg
    -> View id val msg
customizeGroup viewFunc (View ({ attributes } as view)) =
    View { view | attributes = { attributes | groupView = viewFunc } }


{-| Customize the positioning, and appearance of each of the inputs of a repeatable
group of inputs and the and the button to add new inputs.

To customize the template used to add a new input see
[customizeRepeatingField](#customizeRepeatingField).

    view : View String val ()
    view =
        Field.repeatable [ Field.identifier "People" ]
            (Field.group []
                [ [ Field.text [ Field.label "Name" ]
                  , Field.text [ Field.label "Last Name" ]
                  ]
                ]
            )
            []
            |> View.fromField (always ())
            |> customizeRepeatableFields
                (\{ legendText, inputs, addFieldsButton } ->
                    Html.fieldset []
                        [ case legendText of
                            Just str ->
                                Html.legend [] [ Html.text str ]

                            Nothing ->
                                Html.text ""
                        , Html.div [] inputs
                        , addFieldsButton []
                        ]
                )

-}
customizeRepeatableFields :
    ({ legendText : Maybe String
     , fields : List (Html msg)
     , addFieldsButton : List (Attribute msg) -> Html msg
     , errors : List String
     , advanced :
        { identifier : Maybe id
        , addFieldsButtonOnClick : Maybe msg
        , addFieldsButtonCopy : String
        }
     }
     -> Html msg
    )
    -> View id val msg
    -> View id val msg
customizeRepeatableFields viewFunc (View ({ attributes, field } as view)) =
    View
        { view
            | attributes =
                { attributes
                    | repeatableFieldsGroupView =
                        \params ->
                            let
                                unwrappedField =
                                    Tree.value field
                            in
                            viewFunc
                                { legendText = params.legendText
                                , fields = params.fields
                                , addFieldsButton = params.addFieldsButton << toAttrs
                                , errors = params.errors
                                , advanced =
                                    { identifier = unwrappedField.identifier
                                    , addFieldsButtonOnClick = params.addFieldsButtonOnClick
                                    , addFieldsButtonCopy = unwrappedField.addFieldsButtonCopy
                                    }
                                }
                }
        }


{-| Customize the rendering of each of the elements of a repeatable group of
inputs.

To customize the group of inputs see
[customizeRepeatableFields](#customizeRepeatableFields).

    view : View String val ()
    view =
        Field.repeatable [ Field.identifier "People" ]
            (Field.group []
                [ [ Field.text [ Field.label "Name" ]
                  , Field.text [ Field.label "Last Name" ]
                  ]
                ]
            )
            []
            |> View.fromField (always ())
            |> customizeRepeatableField
                (\{ input, removeFieldsButton } ->
                    Html.div
                        [ Attributes.class "group-repeat" ]
                        [ input, removeFieldsButton [] ]
                )

-}
customizeRepeatingFieldTemplate :
    ({ field : Html msg
     , removeFieldsButton : List (Attribute msg) -> Html msg
     , advanced :
        { identifier : Maybe id
        , index : Int
        , removeFieldsButtonOnClick : Maybe msg
        , removeFieldsButtonCopy : String
        }
     }
     -> Html msg
    )
    -> View id val msg
    -> View id val msg
customizeRepeatingFieldTemplate viewFunc (View ({ attributes, field } as view)) =
    View
        { view
            | attributes =
                { attributes
                    | repeatableFieldView =
                        \params ->
                            viewFunc
                                { field = params.field
                                , removeFieldsButton = params.removeFieldsButton << toAttrs
                                , advanced =
                                    { identifier = Tree.value field |> .identifier
                                    , removeFieldsButtonOnClick = params.removeFieldsButtonOnClick
                                    , index = params.index
                                    , removeFieldsButtonCopy = params.removeFieldsButtonCopy
                                    }
                                }
                }
        }


{-| Represents an attribute that can be applied to an element.
-}
type Attribute msg
    = Attribute (Internal.View.UserAttributes -> Internal.View.UserAttributes)


toAttrs : List (Attribute msg) -> Internal.View.UserAttributes
toAttrs =
    List.foldl (\(Attribute f) -> f) Internal.View.defaultAttributes


{-| Apply a conditional list of classes
-}
class : String -> Attribute msg
class classStr =
    classList [ ( classStr, True ) ]


{-| Apply a conditional list of classes
-}
classList : List ( String, Bool ) -> Attribute msg
classList classTuple =
    Attribute (\attrs -> { attrs | classList = classTuple ++ attrs.classList })


{-| Apply a style
-}
style : String -> String -> Attribute msg
style key val =
    Attribute (\attrs -> { attrs | styles = ( key, val ) :: attrs.styles })


{-| -}
type InputType
    = Text
    | TextArea
    | Email
    | Password
    | StrictAutocomplete
    | Integer
    | Float
    | Month
    | Date
    | Select
    | Radio
    | Checkbox


fieldTypeToInputType : Internal.Field.FieldType id val err -> InputType
fieldTypeToInputType inputType =
    case inputType of
        Internal.Field.Text ->
            Text

        Internal.Field.TextArea ->
            TextArea

        Internal.Field.Password ->
            Password

        Internal.Field.StrictAutocomplete ->
            StrictAutocomplete

        Internal.Field.Email ->
            Email

        Internal.Field.Integer ->
            Integer

        Internal.Field.Float ->
            Float

        Internal.Field.Month ->
            Month

        Internal.Field.Date ->
            Date

        Internal.Field.Select ->
            Select

        Internal.Field.Radio ->
            Radio

        Internal.Field.Checkbox ->
            Checkbox

        Internal.Field.Repeatable _ ->
            Text

        Internal.Field.Group ->
            Text
