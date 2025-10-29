module FormToolkit.View exposing
    ( View, fromField, toHtml
    , partial
    , Attribute, class, classList, style
    , customizeErrors, customizeFields
    , customizeGroups, customizeRepeatableFields, customizeRepeatingFieldTemplates
    )

{-|


# View

@docs View, fromField, toHtml
@docs partial


# View customizations


## Attributes

@docs Attribute, class, classList, style


## Markup customization

@docs customizeErrors, customizeFields
@docs customizeGroups, customizeRepeatableFields, customizeRepeatingFieldTemplates

-}

import FormToolkit.Error exposing (Error)
import FormToolkit.Field as Field exposing (Attributes, Field(..), Msg)
import FormToolkit.Value exposing (Value(..))
import Html exposing (Html)
import Internal.Field
import Internal.View


{-| A view is a way to configure the generated markup for an `Field` or group
of fields.
-}
type View id msg
    = View (Internal.View.View id msg)


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
fromField : (Msg id -> msg) -> Field id -> View id msg
fromField toMsg (Field field) =
    View
        (Internal.View.init
            { events =
                { onChange = \id path value cursorPos -> toMsg (Field.InputChanged id path value cursorPos)
                , onCheck = \id path checked -> toMsg (Field.OnCheck id path checked)
                , onFocus = \id path -> toMsg (Field.InputFocused id path)
                , onBlur = \id path -> toMsg (Field.InputBlured id path)
                , onAdd = \id path -> toMsg (Field.InputsAdded id path)
                , onRemove = \id path -> toMsg (Field.InputsRemoved id path)
                }
            , path = []
            , field = field
            }
        )


{-| Render a view
-}
toHtml : View id msg -> Html msg
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
partial : id -> View id msg -> Maybe (View id msg)
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
            |> View.customizeErrors
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

                        ( Field.Select, IsBlank _ ) ->
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
customizeErrors :
    ({ attributes : Attributes id
     , error : Error id
     }
     -> String
    )
    -> View id msg
    -> View id msg
customizeErrors viewFunc (View ({ attributes } as view)) =
    View
        { view
            | attributes =
                { attributes
                    | errorToString =
                        \attrs error ->
                            viewFunc
                                { attributes = attrs
                                , error = error
                                }
                }
        }


{-| Provide a function to override the rendering of a field.

`label` and `input` are functions that take a list of
[Attribute](#Attribute)s.

Use `params` for a greater level of customization of the field.
It is possible to target specific fields by `InputType`, or `identifier`.

The example bellow would render the input exactly as it normaly renders :P

    view : View id val ()
    view =
        Field.text [ Field.label "Name" ]
            |> View.fromField (always ())
            |> customizeFields
                (\{ isRequired, label, fieldHtml, errors, hint } ->
                    Html.div
                        [ Attributes.class "field"
                        , Attributes.classList [ ( "required", isRequired ) ]
                        ]
                        [ -- ↓ call with ↓ Attribute list
                          label [ class "input-label" ]
                        , Html.div
                            [ Attributes.class "input-wrapper" ]
                            [ -- ↓ same here, label `for` already references the input
                              fieldHtml []
                            ]
                        , case errors of
                            err :: _ ->
                                Html.p [ Attributes.class "errors" ] [ Html.text err ]

                            [] ->
                                hint []
                        ]
                )

-}
customizeFields :
    ({ isRequired : Bool
     , labelHtml : List (Attribute msg) -> Html msg
     , fieldHtml : List (Attribute msg) -> Html msg
     , hintHtml : List (Attribute msg) -> Html msg
     , errors : List String
     , class : String
     , attributes : Field.Attributes id
     , events :
        { inputOnChange : Value -> { selectionStart : Int, selectionEnd : Int } -> msg
        , inputOnCheck : Bool -> msg
        , inputOnBlur : msg
        , inputOnFocus : msg
        }
     }
     -> Maybe (Html msg)
    )
    -> View id msg
    -> View id msg
customizeFields viewFunc (View ({ attributes } as view)) =
    let
        defaultViewFunction =
            attributes.fieldView

        fieldView =
            \params ->
                case
                    viewFunc
                        { isRequired = params.isRequired
                        , labelHtml = toAttrs >> params.labelHtml
                        , fieldHtml = toAttrs >> params.inputHtml
                        , hintHtml = toAttrs >> params.hintHtml
                        , errors = params.errors
                        , class = String.join " " params.attributes.classList
                        , attributes = params.attributes
                        , events =
                            { inputOnChange =
                                \(Value val) cursorPos -> attributes.onChange params.attributes.identifier params.path val cursorPos
                            , inputOnCheck = \checked -> attributes.onCheck params.attributes.identifier params.path checked
                            , inputOnBlur = attributes.onBlur params.attributes.identifier params.path
                            , inputOnFocus = attributes.onFocus params.attributes.identifier params.path
                            }
                        }
                of
                    Just html ->
                        html

                    Nothing ->
                        defaultViewFunction params
    in
    View
        { view
            | attributes =
                { attributes
                    | fieldView = fieldView
                    , checkboxFieldView = fieldView
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
customizeGroups :
    ({ legendText : Maybe String
     , fields : List (Html msg)
     , identifier : Maybe id
     , errors : List String
     , class : String
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
customizeGroups viewFunc (View ({ attributes } as view)) =
    View { view | attributes = { attributes | groupView = viewFunc } }


{-| Customize the positioning, and appearance of each of the inputs of a repeatable
group of inputs and the and the button to add new inputs.

To customize the template used to add a new input see
[customizeRepeatingFieldTemplates](#customizeRepeatingFieldTemplates).

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
     , class : String
     , params :
        { identifier : Maybe id
        , addFieldsButtonOnClick : Maybe msg
        , addFieldsButtonCopy : String
        }
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
customizeRepeatableFields viewFunc (View ({ attributes } as view)) =
    View
        { view
            | attributes =
                { attributes
                    | repeatableFieldsGroupView =
                        \params ->
                            viewFunc
                                { legendText = params.legendText
                                , fields = params.fields
                                , addFieldsButton = params.addFieldsButton << toAttrs
                                , errors = params.errors
                                , class = String.join " " params.attributes.classList
                                , params =
                                    { identifier = params.attributes.identifier
                                    , addFieldsButtonOnClick = params.addFieldsButtonOnClick
                                    , addFieldsButtonCopy = params.attributes.addFieldsButtonCopy
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
customizeRepeatingFieldTemplates :
    ({ field : Html msg
     , removeFieldsButton : List (Attribute msg) -> Html msg
     , params :
        { identifier : Maybe id
        , index : Int
        , removeFieldsButtonOnClick : Maybe msg
        , removeFieldsButtonCopy : String
        }
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
customizeRepeatingFieldTemplates viewFunc (View ({ attributes } as view)) =
    View
        { view
            | attributes =
                { attributes
                    | repeatableFieldView =
                        \params ->
                            viewFunc
                                { field = params.field
                                , removeFieldsButton = params.removeFieldsButton << toAttrs
                                , params =
                                    { identifier = params.attributes.identifier
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


{-| Apply a CSS class
-}
class : String -> Attribute msg
class classStr =
    classList [ ( classStr, True ) ]


{-| Apply a conditional list of CSS classes
-}
classList : List ( String, Bool ) -> Attribute msg
classList classTuple =
    Attribute (\attrs -> { attrs | classList = classTuple ++ attrs.classList })


{-| Apply a style
-}
style : String -> String -> Attribute msg
style key val =
    Attribute (\attrs -> { attrs | styles = ( key, val ) :: attrs.styles })
