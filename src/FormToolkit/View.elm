module FormToolkit.View exposing
    ( View, fromInput, toHtml
    , partial
    , InputType(..), Attribute, class, classList, style
    , customizeError, customizeInput
    , customizeGroup, customizeRepeatableInputsGroup, customizeRepeatableInput
    )

{-|


# View

@docs View, fromInput, toHtml
@docs partial


# View customizations


## Attributes

@docs InputType, Attribute, class, classList, style


## Markup customization

@docs customizeError, customizeInput
@docs customizeGroup, customizeRepeatableInputsGroup, customizeRepeatableInput

-}

import FormToolkit.Decode exposing (Error(..))
import FormToolkit.Value as Value exposing (Value)
import Html exposing (Html)
import Html.Attributes as Attributes exposing (attribute)
import Html.Events as Events
import Internal.Input as Input
    exposing
        ( Input
        , Msg(..)
        , Status(..)
        )
import Internal.Value
import Json.Decode
import RoseTree.Tree as Tree


type alias Input id val =
    Input.Input id val (Error id val)


type alias Msg id val =
    Input.Msg id val


type alias ViewAttributes id val msg =
    { onChange : Msg id val -> msg
    , errorToString :
        { inputType : InputType
        , error : Error id val
        }
        -> String
    , inputView : InputView id val msg -> Html msg
    , groupView : GroupView id msg -> Html msg
    , repeatableInputsGroupView : RepeatableInputsGroupView id msg -> Html msg
    , repeatableInputView : RepeatableInputView id msg -> Html msg
    }


type alias InputView id val msg =
    { isRequired : Bool
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
        , labelText : Maybe String
        , hintText : Maybe String
        , idString : String
        }
    }


type alias GroupView id msg =
    { legendText : Maybe String
    , inputs : List (Html msg)
    , identifier : Maybe id
    , errors : List String
    }


type alias RepeatableInputsGroupView id msg =
    { legendText : Maybe String
    , inputs : List (Html msg)
    , addInputButton : List (Attribute msg) -> Html msg
    , errors : List String
    , advanced :
        { identifier : Maybe id
        , addInputButtonOnClick : Maybe msg
        , addInputButtonCopy : String
        }
    }


type alias RepeatableInputView id msg =
    { input : Html msg
    , removeInputButton : List (Attribute msg) -> Html msg
    , advanced :
        { identifier : Maybe id
        , index : Int
        , removeInputButtonOnClick : Maybe msg
        , removeInputButtonCopy : String
        }
    }


{-| Represents a view for an [Input](FormToolkit.Input#Input) for the purposes
of customized rendering.
-}
type View id val msg
    = View (Input id val) (List Int) (ViewAttributes id val msg)


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
    View input
        []
        { onChange = onChange
        , errorToString = errorToString
        , inputView = inputView
        , groupView = groupView
        , repeatableInputsGroupView = repeatableInputsGroupView
        , repeatableInputView = repeatableInputView
        }


{-| Render a view
-}
toHtml : View id val msg -> Html msg
toHtml (View input path attributes) =
    toHtmlHelp attributes path input


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
partial id (View input _ attributes) =
    findNode id input
        |> Maybe.map (\( found, path ) -> View found path attributes)


findNode : id -> Input id val -> Maybe ( Input id val, List Int )
findNode id tree =
    Tree.foldWithPath
        (\path node foundPath ->
            if .identifier (Tree.value node) == Just id then
                Just ( node, path )

            else
                foundPath
        )
        Nothing
        tree


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
    ({ inputType : InputType
     , error : Error id val
     }
     -> String
    )
    -> View id val msg
    -> View id val msg
customizeError viewFunc (View input path params) =
    View input path { params | errorToString = viewFunc }


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
        , labelText : Maybe String
        , hintText : Maybe String
        , idString : String
        }
     }
     -> Html msg
    )
    -> View id val msg
    -> View id val msg
customizeInput viewFunc (View input path params) =
    View input path { params | inputView = viewFunc }


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
customizeGroup viewFunc (View input path params) =
    View input path { params | groupView = viewFunc }


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
customizeRepeatableInputsGroup viewFunc (View input path params) =
    View input path { params | repeatableInputsGroupView = viewFunc }


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
            |> View.fromInput (always ())
            |> customizeRepeatableInput
                (\{ input, removeInputButton } ->
                    Html.div
                        [ Attributes.class "group-repeat" ]
                        [ input, removeInputButton [] ]
                )

-}
customizeRepeatableInput :
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
customizeRepeatableInput viewFunc (View input path params) =
    View input path { params | repeatableInputView = viewFunc }


toHtmlHelp : ViewAttributes id val msg -> List Int -> Input id val -> Html msg
toHtmlHelp attributes path (tree as input) =
    let
        unwrappedInput =
            Tree.value tree

        wrapInput inputHtml =
            attributes.inputView
                { isRequired = unwrappedInput.isRequired
                , label = toAttrs >> labelToHtml unwrappedInput.label path input
                , input = toAttrs >> inputHtml
                , errors =
                    inputErrors input |> List.map attributes.errorToString
                , hint =
                    \attrList ->
                        case unwrappedInput.hint of
                            Just hintText ->
                                Html.div
                                    (Attributes.class "hint"
                                        :: Attributes.id (hintId input path)
                                        :: userProvidedAttributes (toAttrs attrList)
                                    )
                                    [ Html.text hintText ]

                            Nothing ->
                                Html.text ""
                , advanced =
                    { identifier = unwrappedInput.identifier
                    , inputType = mapInputType unwrappedInput.inputType
                    , inputName = unwrappedInput.name
                    , inputPlaceholder = unwrappedInput.placeholder
                    , inputValue = Value.Value unwrappedInput.value
                    , inputMin = Value.Value unwrappedInput.min
                    , inputMax = Value.Value unwrappedInput.max
                    , inputOptions =
                        List.map (Tuple.mapSecond Value.Value) unwrappedInput.options
                    , inputOnChange =
                        \(Value.Value val) ->
                            InputChanged path val |> attributes.onChange
                    , labelText = unwrappedInput.label
                    , hintText = unwrappedInput.hint
                    , idString = inputId input path
                    }
                }
    in
    case unwrappedInput.inputType of
        Input.Group ->
            groupToHtml attributes path input

        Input.Repeatable _ ->
            repeatableToHtml attributes path input

        Input.Text ->
            wrapInput (inputToHtml attributes "text" path input [])

        Input.Email ->
            wrapInput (inputToHtml attributes "email" path input [])

        Input.Password ->
            wrapInput (inputToHtml attributes "password" path input [])

        Input.TextArea ->
            wrapInput (textAreaToHtml attributes path input)

        Input.Integer ->
            inputToHtml attributes "number" path input [ Attributes.step "1" ]
                |> wrapInput

        Input.Float ->
            inputToHtml attributes "number" path input [ Attributes.step "1" ]
                |> wrapInput

        Input.Date ->
            wrapInput (inputToHtml attributes "date" path input [])

        Input.Month ->
            wrapInput (inputToHtml attributes "month" path input [])

        Input.Select ->
            wrapInput (selectToHtml attributes path input)

        Input.Radio ->
            wrapInput (radioToHtml attributes path input)

        Input.Checkbox ->
            wrapInput (checkboxToHtml attributes path input)


labelToHtml : Maybe String -> List Int -> Input id val -> (UserAttributes -> Html msg)
labelToHtml label path input element =
    case label of
        Just str ->
            Html.label
                (Attributes.for (inputId input path)
                    :: Attributes.id (labelId input path)
                    :: userProvidedAttributes element
                )
                [ Html.text str ]

        Nothing ->
            Html.text ""


groupToHtml : ViewAttributes id val msg -> List Int -> Input id val -> Html msg
groupToHtml attributes path (tree as input) =
    let
        { identifier, label } =
            Tree.value tree
    in
    attributes.groupView
        { legendText = label
        , inputs =
            Tree.children tree
                |> List.indexedMap
                    (\idx ->
                        toHtmlHelp attributes (path ++ [ idx ])
                    )
        , identifier = identifier
        , errors = inputErrors input |> List.map attributes.errorToString
        }


repeatableToHtml : ViewAttributes id val msg -> List Int -> Input id val -> Html msg
repeatableToHtml attributes path (tree as input) =
    let
        unwrappedInput =
            Tree.value tree

        children =
            Tree.children tree

        childrenCount =
            List.length children

        inputsView idx child =
            let
                childPath =
                    path ++ [ idx ]

                removeInputButtonOnClick =
                    attributes.onChange (InputsRemoved childPath)

                removeInputButtonCopy =
                    Tree.value child |> .removeInputsButtonCopy

                removeInputButtonEnabled =
                    childrenCount > unwrappedInput.repeatableMin
            in
            attributes.repeatableInputView
                { input = toHtmlHelp attributes childPath child
                , removeInputButton =
                    \attrList ->
                        Html.button
                            (Attributes.class "remove-fields"
                                :: Attributes.disabled (not removeInputButtonEnabled)
                                :: Events.preventDefaultOn "click"
                                    (Json.Decode.succeed
                                        ( removeInputButtonOnClick
                                        , True
                                        )
                                    )
                                :: userProvidedAttributes (toAttrs attrList)
                            )
                            [ Html.text removeInputButtonCopy
                            ]
                , advanced =
                    { identifier = Tree.value tree |> .identifier
                    , removeInputButtonOnClick =
                        if removeInputButtonEnabled then
                            Just removeInputButtonOnClick

                        else
                            Nothing
                    , index = idx
                    , removeInputButtonCopy = removeInputButtonCopy
                    }
                }

        addInputButtonEnabled =
            case unwrappedInput.repeatableMax of
                Just max ->
                    childrenCount < max

                Nothing ->
                    True
    in
    repeatableInputsGroupView
        { legendText = unwrappedInput.label
        , inputs = List.indexedMap inputsView children
        , addInputButton =
            \attrList ->
                Html.button
                    (Attributes.class "add-fields"
                        :: Attributes.disabled (not addInputButtonEnabled)
                        :: Events.preventDefaultOn "click"
                            (Json.Decode.succeed
                                ( attributes.onChange (InputsAdded path)
                                , True
                                )
                            )
                        :: userProvidedAttributes (toAttrs attrList)
                    )
                    [ Html.text unwrappedInput.addInputsButtonCopy ]
        , errors =
            inputErrors input |> List.map attributes.errorToString
        , advanced =
            { identifier = unwrappedInput.identifier
            , addInputButtonOnClick =
                if addInputButtonEnabled then
                    Just (attributes.onChange (InputsAdded path))

                else
                    Nothing
            , addInputButtonCopy = unwrappedInput.addInputsButtonCopy
            }
        }


inputToHtml :
    ViewAttributes id val msg
    -> String
    -> List Int
    -> Input id val
    -> List (Html.Attribute msg)
    -> (UserAttributes -> Html msg)
inputToHtml { onChange } inputType path input htmlAttrs element =
    let
        unwrappedInput =
            Tree.value input

        inputHtml =
            Html.input
                (List.concat
                    [ htmlAttrs
                    , Attributes.type_ inputType
                        :: valueAttribute Attributes.value unwrappedInput.value
                        :: onInputChanged onChange input path
                        :: textInputHtmlAttributes onChange path input
                    , userProvidedAttributes element
                    ]
                )
                []
    in
    if Input.isAutocompleteable input then
        Html.div
            []
            [ inputHtml
            , Html.datalist
                [ Attributes.id (datalistId input path)
                , Attributes.attribute "role" "listbox"
                ]
                (List.map
                    (\( opt, _ ) -> Html.option [ Attributes.value opt ] [])
                    unwrappedInput.options
                )
            ]

    else
        inputHtml


textAreaToHtml :
    ViewAttributes id val msg
    -> List Int
    -> Input id val
    -> (UserAttributes -> Html msg)
textAreaToHtml { onChange } path input element =
    let
        valueStr =
            Tree.value input
                |> .value
                |> Internal.Value.toString
                |> Maybe.withDefault ""
    in
    Html.div
        [ Attributes.class "grow-wrap"
        , Attributes.attribute "data-replicated-value" valueStr
        ]
        [ Html.textarea
            (List.concat
                [ onInputChanged onChange input path
                    :: Attributes.value valueStr
                    :: textInputHtmlAttributes onChange path input
                , userProvidedAttributes element
                ]
            )
            []
        ]


selectToHtml :
    ViewAttributes id val msg
    -> List Int
    -> Input id val
    -> (UserAttributes -> Html msg)
selectToHtml { onChange } path (tree as input) element =
    let
        unwappedInput =
            Tree.value tree
    in
    Html.select
        (Attributes.id (inputId input path)
            :: Attributes.required unwappedInput.isRequired
            :: nameAttribute input
            :: ariaLabeledByAttribute input path
            :: ariaDescribedByAttribute input path
            :: ariaInvalidAttribute input
            :: onInputChanged onChange input path
            :: onInputFocused onChange path
            :: onInputBlured onChange path
            :: userProvidedAttributes element
        )
        (Html.option [] []
            :: List.indexedMap
                (\index ( optionText, optionValue ) ->
                    Html.option
                        [ Attributes.selected (optionValue == unwappedInput.value)
                        , Attributes.value (String.fromInt index)
                        ]
                        [ Html.text optionText ]
                )
                unwappedInput.options
        )


radioToHtml :
    ViewAttributes id val msg
    -> List Int
    -> Input id val
    -> (UserAttributes -> Html msg)
radioToHtml { onChange } path input element =
    let
        unwrappedInput =
            Tree.value input
    in
    Html.div
        [ Attributes.class "radios"
        , Attributes.attribute "role" "radiogroup"
        , ariaLabeledByAttribute input path
        , ariaDescribedByAttribute input path
        ]
        (List.indexedMap
            (\index ( optionText, optionValue ) ->
                Html.div
                    []
                    [ Html.input
                        (Attributes.id (radioOptionId input (path ++ [ index ]))
                            :: Attributes.checked (optionValue == unwrappedInput.value)
                            :: Attributes.required unwrappedInput.isRequired
                            :: Attributes.value (String.fromInt index)
                            :: Attributes.type_ "radio"
                            :: nameAttribute input
                            :: onInputChanged onChange input path
                            :: onInputFocused onChange path
                            :: onInputBlured onChange path
                            :: ariaInvalidAttribute input
                            :: userProvidedAttributes element
                        )
                        []
                    , Html.label
                        [ Attributes.for (radioOptionId input (path ++ [ index ])) ]
                        [ Html.text optionText ]
                    ]
            )
            unwrappedInput.options
        )


checkboxToHtml :
    ViewAttributes id val msg
    -> List Int
    -> Input id val
    -> (UserAttributes -> Html msg)
checkboxToHtml { onChange } path input element =
    Html.input
        (List.concat
            [ Attributes.type_ "checkbox"
                :: (Tree.value input
                        |> .value
                        |> Internal.Value.toBool
                        |> Maybe.map Attributes.checked
                        |> Maybe.withDefault (Attributes.class "")
                   )
                :: Events.onCheck
                    (Internal.Value.fromBool
                        >> InputChanged path
                        >> onChange
                    )
                :: textInputHtmlAttributes onChange path input
            , userProvidedAttributes element
            ]
        )
        []


valueAttribute :
    (String -> Html.Attribute msg)
    -> Internal.Value.Value val
    -> Html.Attribute msg
valueAttribute f inputValue =
    Internal.Value.toString inputValue
        |> Maybe.map f
        |> Maybe.withDefault (Attributes.class "")


textInputHtmlAttributes : (Msg id val -> msg) -> List Int -> Input id val -> List (Html.Attribute msg)
textInputHtmlAttributes onChange path input =
    List.concat
        [ if Input.isAutocompleteable input then
            [ Attributes.autocomplete True
            , Attributes.list (datalistId input path)
            ]

          else
            [ Attributes.autocomplete False ]
        , [ Attributes.placeholder (Maybe.withDefault "" (Input.placeholder input))
          , Attributes.id (inputId input path)
          , Attributes.required (Input.isRequired input)
          , nameAttribute input
          , valueAttribute Attributes.min (Input.min input)
          , valueAttribute Attributes.max (Input.max input)
          , onInputFocused onChange path
          , onInputBlured onChange path
          , ariaDescribedByAttribute input path
          , ariaInvalidAttribute input
          ]
        ]


onInputChanged : (Msg id val -> c) -> Input id val -> List Int -> Html.Attribute c
onInputChanged tagger input path =
    Events.onInput (Input.inputChanged input path >> tagger)


onInputFocused : (Msg id val -> msg) -> List Int -> Html.Attribute msg
onInputFocused tagger path =
    Events.onFocus (tagger (InputFocused path))


onInputBlured : (Msg id val -> msg) -> List Int -> Html.Attribute msg
onInputBlured tagger path =
    Events.onBlur (tagger (InputBlured path))


inputId : Input id val -> List Int -> String
inputId input path =
    String.join "-" (Input.inputIdString input :: List.map String.fromInt path)


labelId : Input id val -> List Int -> String
labelId input path =
    inputId input path ++ "-label"


hintId : Input id val -> List Int -> String
hintId input path =
    inputId input path ++ "-hint"


radioOptionId : Input id val -> List Int -> String
radioOptionId input path =
    inputId input path ++ "-option"


datalistId : Input id val -> List Int -> String
datalistId input path =
    inputId input path ++ "-datalist"


inputErrors : Input id val -> List { error : Error id val, inputType : InputType }
inputErrors tree =
    let
        { errors, status, inputType } =
            Tree.value tree

        errorRecords =
            List.map
                (\error ->
                    { error = error
                    , inputType = mapInputType inputType
                    }
                )
                errors
    in
    case ( status, inputType ) of
        ( Touched, _ ) ->
            errorRecords

        ( _, Input.Repeatable _ ) ->
            errorRecords

        ( _, Input.Group ) ->
            errorRecords

        _ ->
            []


groupView : GroupView id msg -> Html msg
groupView { inputs, legendText, errors } =
    Html.fieldset []
        (List.concat
            [ (case legendText of
                Just str ->
                    Html.legend [] [ Html.text str ]

                Nothing ->
                    Html.text ""
              )
                :: inputs
            , [ viewErrors errors ]
            ]
        )


repeatableInputsGroupView : RepeatableInputsGroupView id msg -> Html msg
repeatableInputsGroupView { legendText, addInputButton, inputs, errors } =
    Html.fieldset []
        [ case legendText of
            Just str ->
                Html.legend [] [ Html.text str ]

            Nothing ->
                Html.text ""
        , Html.div [] inputs
        , addInputButton []
        , viewErrors errors
        ]


repeatableInputView : RepeatableInputView id msg -> Html msg
repeatableInputView { input, removeInputButton } =
    Html.div
        [ Attributes.class "group-repeat" ]
        [ input
        , removeInputButton []
        ]


inputView : InputView id val msg -> Html msg
inputView { isRequired, label, input, errors, hint } =
    Html.div
        [ Attributes.class "field"
        , Attributes.classList [ ( "required", isRequired ) ]
        ]
        [ label []
        , Html.div
            [ Attributes.class "input-wrapper" ]
            [ input [] ]
        , case errors of
            [] ->
                hint []

            _ ->
                viewErrors errors
        ]


viewErrors : List String -> Html msg
viewErrors errors =
    case errors of
        err :: _ ->
            Html.p [ Attributes.class "errors" ] [ Html.text err ]

        [] ->
            Html.text ""


{-| Represents an attribute that can be applied to an element.
-}
type Attribute msg
    = Attribute (UserAttributes -> UserAttributes)


type alias UserAttributes =
    { classList : List ( String, Bool )
    , styles : List ( String, String )
    }


{-| -}
type InputType
    = Text
    | TextArea
    | Password
    | Email
    | Integer
    | Float
    | Month
    | Date
    | Select
    | Radio
    | Checkbox


mapInputType : Input.InputType id val err -> InputType
mapInputType inputType =
    case inputType of
        Input.Text ->
            Text

        Input.TextArea ->
            TextArea

        Input.Password ->
            Password

        Input.Email ->
            Email

        Input.Integer ->
            Integer

        Input.Float ->
            Float

        Input.Month ->
            Month

        Input.Date ->
            Date

        Input.Select ->
            Select

        Input.Radio ->
            Radio

        Input.Checkbox ->
            Checkbox

        Input.Repeatable _ ->
            Text

        Input.Group ->
            Text


toAttrs : List (Attribute msg) -> UserAttributes
toAttrs =
    List.foldl (\(Attribute f) attrs -> f attrs) { classList = [], styles = [] }


{-| Apply a conditional list of classes
-}
class : String -> Attribute msg
class classStr =
    classList [ ( classStr, True ) ]


{-| Apply a conditional list of classes
-}
classList : List ( String, Bool ) -> Attribute msg
classList classTuple =
    Attribute
        (\attrs ->
            { attrs | classList = classTuple ++ attrs.classList }
        )


{-| Apply a style
-}
style : String -> String -> Attribute msg
style key val =
    Attribute (\attrs -> { attrs | styles = ( key, val ) :: attrs.styles })


userProvidedAttributes : UserAttributes -> List (Html.Attribute msg)
userProvidedAttributes element =
    Attributes.classList element.classList
        :: List.map (\( k, v ) -> Attributes.style k v) element.styles


nameAttribute : Input id val -> Html.Attribute msg
nameAttribute input =
    Input.name input
        |> Maybe.map Attributes.name
        |> Maybe.withDefault (Attributes.class "")


ariaDescribedByAttribute : Input id val -> List Int -> Html.Attribute msg
ariaDescribedByAttribute input path =
    Input.hint input
        |> Maybe.map (\_ -> attribute "aria-describedby" (hintId input path))
        |> Maybe.withDefault (Attributes.class "")


ariaLabeledByAttribute : Input id val -> List Int -> Html.Attribute msg
ariaLabeledByAttribute input path =
    Input.label input
        |> Maybe.map (\_ -> attribute "aria-labelledby" (labelId input path))
        |> Maybe.withDefault (Attributes.class "")


ariaInvalidAttribute : Input id val -> Html.Attribute msg
ariaInvalidAttribute input =
    if List.isEmpty (Input.errors input) then
        Attributes.class ""

    else
        attribute "aria-invalid" "true"


errorToString : { inputType : InputType, error : Error id val } -> String
errorToString { error } =
    let
        toString =
            Value.toString >> Maybe.withDefault ""
    in
    case error of
        ValueTooLarge _ data ->
            "Should be lesser than " ++ toString data.max

        ValueTooSmall _ data ->
            "Should be greater than " ++ toString data.min

        ValueNotInRange _ data ->
            "Should be between " ++ toString data.min ++ " and " ++ toString data.max

        IsBlank _ ->
            "Should be provided"

        IsGroupNotInput _ ->
            "A group cannot have a value but the decoder is attempting to read the value"

        CustomError _ message ->
            message

        _ ->
            "Couldn't parse"
