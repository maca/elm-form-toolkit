module FormToolkit.View exposing
    ( View, fromInput, toHtml
    , partial
    , Attribute, class, classList, style
    , InputType(..)
    , customizeError, customizeInput
    , customizeGroup, customizeRepeatableInputsGroup, customizeRepeatingInput
    )

{-|


# View

@docs View, fromInput, toHtml
@docs partial


# View customizations


## Attributes

@docs Attribute, class, classList, style


## Markup customization

@docs InputType
@docs customizeError, customizeInput
@docs customizeGroup, customizeRepeatableInputsGroup, customizeRepeatingInput

-}

import FormToolkit.Decode exposing (Error)
import FormToolkit.Input exposing (Input)
import FormToolkit.Value exposing (Value(..))
import Html exposing (Html)
import Internal.Input exposing (Msg(..))
import Internal.View
import RoseTree.Tree as Tree


{-| Represents a view for an [Input](FormToolkit.Input#Input) for the purposes
of customized rendering.
-}
type View id val msg
    = View (Internal.View.View id val msg)


{-| Construct a view from an [Input](FormToolkit.Input#Input).

    view : Html (Never -> a)
    view =
        Input.group []
            [ Input.text [ Input.label "First Name" ]
            , Input.text [ Input.label "Last Name" ]
            ]
            |> View.fromInput (always never)
            |> View.toHtml

-}
fromInput : (Msg id val -> msg) -> Input id val -> View id val msg
fromInput onChange input =
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
            , input = input
            }
        )


{-| Render a view
-}
toHtml : View id val msg -> Html msg
toHtml (View view) =
    Internal.View.toHtml view


{-| A partial view referenced by [identifier](FormToolkit.Input#identifier).
Maybe you want to render segments of the same form in different UI sections.

    Input.group []
        [ Input.text
            [ Input.identifier "FirstName"
            , Input.label "First Name"
            ]
        , Input.text
            [ Input.identifier "LastName"
            , Input.label "Last Name"
            ]
        ]
        |> View.fromInput (always never)
        |> View.partial "FirstName"
        == Just
            (Input.text
                [ Input.identifier "FirstName"
                , Input.label "First Name"
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
        Input.group []
            [ Input.text
                [ Input.label "Name"
                , Input.identifier Name
                , Input.required
                ]
            , Input.text
                [ Input.label "Temperature"
                , Input.identifier Temperature
                , Input.min 20
                , Input.max 35
                , Input.required
                ]
            , Input.select
                [ Input.label "Flavour"
                , Input.identifier Flavour
                , Input.required
                , Input.options
                    [ ( "Banana", Value.string "banana" )
                    , ( "Strawbery", Value.string "strawberry" )
                    , ( "Chocolate", Value.string "chocolate" )
                    ]
                ]
            ]
            |> View.fromInput (always ())
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
customizeError viewFunc (View ({ attributes, input } as view)) =
    View
        { view
            | attributes =
                { attributes
                    | errorToString =
                        \error ->
                            let
                                unwrappedInput =
                                    Tree.value input
                            in
                            viewFunc
                                { inputType = mapInputType unwrappedInput.inputType
                                , error = error
                                }
                }
        }


{-| Provide a function to override the rendering of an input.

`label` and `input` are functions that take a list of
[Attribute](#Attribute)s.

Use `advanced` parameters for a greater level of customization of the input
and label.
It is possible to target specific [input types](#InputType) or
[specific inputs](FormToolkit.Input#identifier) by using `inputType` or
`identifier` parameters.

The example bellow would render the input exactly as it normaly renders :P

    view : View id val ()
    view =
        Input.text [ Input.label "Name" ]
            |> View.fromInput (always ())
            |> customizeInput
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
customizeInput :
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
customizeInput viewFunc (View ({ attributes, input } as view)) =
    View
        { view
            | attributes =
                { attributes
                    | inputView =
                        \params ->
                            let
                                unwrappedInput =
                                    Tree.value input
                            in
                            viewFunc
                                { isRequired = params.isRequired
                                , label = toAttrs >> params.label
                                , input = toAttrs >> params.input
                                , hint = toAttrs >> params.input
                                , errors = []
                                , advanced =
                                    { identifier = unwrappedInput.identifier
                                    , inputType = mapInputType unwrappedInput.inputType
                                    , inputName = unwrappedInput.name
                                    , inputPlaceholder = unwrappedInput.placeholder
                                    , inputValue = Value unwrappedInput.value
                                    , inputMin = Value unwrappedInput.min
                                    , inputMax = Value unwrappedInput.max
                                    , inputOptions =
                                        List.map (Tuple.mapSecond Value) unwrappedInput.options
                                    , inputOnChange =
                                        \(Value val) -> attributes.onChange params.path val
                                    , inputOnBlur = attributes.onBlur params.path
                                    , inputOnFocus = attributes.onFocus params.path
                                    , labelText = unwrappedInput.label
                                    , hintText = unwrappedInput.hint
                                    , idString = Internal.View.inputId input params.path
                                    }
                                }
                }
        }


{-| Provide a function to customize the rendering of a group of inputs.

    view : View id val ()
    view =
        Input.group []
            [ Input.text [ Input.label "Name" ]
            , Input.text [ Input.label "Last Name" ]
            ]
            |> View.fromInput (always ())
            |> customizeInput
                (\{ inputs, legendText } ->
                    Html.fieldset
                        [ Html.Attribute.class "input-group" ]
                        ((case legendText of
                            Just str ->
                                Html.legend [] [ Html.text str ]

                            Nothing ->
                                Html.text ""
                         )
                            :: inputs
                        )
                )

-}
customizeGroup :
    ({ legendText : Maybe String
     , inputs : List (Html msg)
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
group of inputs and the and the button to add new inputs, see
[Input.repeatable](FormToolkit.Input#repeatable).

To customize the template used to add a new input see
[customizeRepeatableInput](#customizeRepeatableInput).

    view : View String val ()
    view =
        Input.repeatable [ Input.identifier "People" ]
            (Input.group []
                [ [ Input.text [ Input.label "Name" ]
                  , Input.text [ Input.label "Last Name" ]
                  ]
                ]
            )
            []
            |> View.fromInput (always ())
            |> customizeRepeatableInputsGroup
                (\{ legendText, inputs, addInputButton } ->
                    Html.fieldset []
                        [ case legendText of
                            Just str ->
                                Html.legend [] [ Html.text str ]

                            Nothing ->
                                Html.text ""
                        , Html.div [] inputs
                        , addInputButton []
                        ]
                )

-}
customizeRepeatableInputsGroup :
    ({ legendText : Maybe String
     , inputs : List (Html msg)
     , addInputButton : List (Attribute msg) -> Html msg
     , errors : List String
     , advanced :
        { identifier : Maybe id
        , addInputButtonOnClick : Maybe msg
        , addInputButtonCopy : String
        }
     }
     -> Html msg
    )
    -> View id val msg
    -> View id val msg
customizeRepeatableInputsGroup viewFunc (View ({ attributes, input } as view)) =
    View
        { view
            | attributes =
                { attributes
                    | repeatableInputsGroupView =
                        \params ->
                            let
                                unwrappedInput =
                                    Tree.value input
                            in
                            viewFunc
                                { legendText = params.legendText
                                , inputs = params.inputs
                                , addInputButton = params.addInputButton << toAttrs
                                , errors = params.errors
                                , advanced =
                                    { identifier = unwrappedInput.identifier
                                    , addInputButtonOnClick = params.addInputButtonOnClick
                                    , addInputButtonCopy = unwrappedInput.addInputsButtonCopy
                                    }
                                }
                }
        }


{-| Customize the rendering of each of the elements of a repeatable group of
inputs, see [Input.repeatable](FormToolkit.Input#repeatable).

To customize the group of inputs see
[customizeRepeatableInputsGroup](#customizeRepeatableInputsGroup).

    view : View String val ()
    view =
        Input.repeatable [ Input.identifier "People" ]
            (Input.group []
                [ [ Input.text [ Input.label "Name" ]
                  , Input.text [ Input.label "Last Name" ]
                  ]
                ]
            )
            []
            |> View.fromInput (always ())
            |> customizeRepeatableInput
                (\{ input, removeInputButton } ->
                    Html.div
                        [ Attributes.class "group-repeat" ]
                        [ input, removeInputButton [] ]
                )

-}
customizeRepeatingInput :
    ({ input : Html msg
     , removeInputButton : List (Attribute msg) -> Html msg
     , advanced :
        { identifier : Maybe id
        , index : Int
        , removeInputButtonOnClick : Maybe msg
        , removeInputButtonCopy : String
        }
     }
     -> Html msg
    )
    -> View id val msg
    -> View id val msg
customizeRepeatingInput viewFunc (View ({ attributes, input } as view)) =
    View
        { view
            | attributes =
                { attributes
                    | repeatableInputView =
                        \params ->
                            viewFunc
                                { input = params.input
                                , removeInputButton = params.removeInputButton << toAttrs
                                , advanced =
                                    { identifier = Tree.value input |> .identifier
                                    , removeInputButtonOnClick = params.removeInputButtonOnClick
                                    , index = params.index
                                    , removeInputButtonCopy = params.removeInputButtonCopy
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


mapInputType : Internal.Input.InputType id val err -> InputType
mapInputType inputType =
    case inputType of
        Internal.Input.Text ->
            Text

        Internal.Input.TextArea ->
            TextArea

        Internal.Input.Password ->
            Password

        Internal.Input.StrictAutocomplete ->
            StrictAutocomplete

        Internal.Input.Email ->
            Email

        Internal.Input.Integer ->
            Integer

        Internal.Input.Float ->
            Float

        Internal.Input.Month ->
            Month

        Internal.Input.Date ->
            Date

        Internal.Input.Select ->
            Select

        Internal.Input.Radio ->
            Radio

        Internal.Input.Checkbox ->
            Checkbox

        Internal.Input.Repeatable _ ->
            Text

        Internal.Input.Group ->
            Text
