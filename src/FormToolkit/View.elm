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
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Internal.Input as Internal
    exposing
        ( Input(..)
        , Msg(..)
        , Status(..)
        )
import Internal.Value
import Json.Decode
import RoseTree.Tree as Tree


type alias Input id val =
    Internal.Input id val (Error id val)


type alias Msg id val =
    Internal.Msg id val


type alias ViewAttributes id val msg =
    { onChange : Msg id val -> msg
    , errorToString : { inputType : InputType, error : Error id val } -> String
    , inputView : InputView id val msg -> Html msg
    , groupView : GroupView id msg -> Html msg
    , repeatableInputsGroupView : RepeatableInputsGroupView id msg -> Html msg
    , repeatableInputView : RepeatableInputView id msg -> Html msg
    }


type alias InputView id val msg =
    { isRequired : Bool
    , label : List (Attribute msg) -> Html msg
    , input : List (Attribute msg) -> Html msg
    , hint : Maybe String
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
        , idString : String
        }
    }


type alias GroupView id msg =
    { legend : List (Attribute msg) -> Html msg
    , inputs : List (Html msg)
    , identifier : Maybe id
    }


type alias RepeatableInputsGroupView id msg =
    { legend : List (Attribute msg) -> Html msg
    , inputs : List (Html msg)
    , addInputButton : List (Attribute msg) -> Html msg
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
findNode id (Input tree) =
    Tree.foldWithPath
        (\path node foundPath ->
            if .identifier (Tree.value node) == Just id then
                Just ( Input node, path )

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

    view : View Fields Never
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
            |> View.fromInput (always never)
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


{-| Customize the rendering of an input by passing a function.
Note that the provided parameters `label` and `input` are functions that take a
list of [Attribute](#Attribute) and build an `Html` element.
`isRequired` indicates that the field will fail validation if no input is present,
`inputType` allows matching inputs of a certain [InputType](#InputType), and `identifier`
allows matching a specific input identified by [identifier](FormToolkit.Input#identifier).

The example bellow would render the input exactly as it normaly renders :P

    view : View id Never
    view =
        Input.text [ Input.label "Name", Input.identifier Name ]
            |> View.fromInput (always never)
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
                            [ -- ↓ same here, label `for` will reference this input
                              input []
                            ]
                        , case errors of
                            err :: _ ->
                                Html.p [ Attributes.class "error" ] [ Html.text err ]

                            [] ->
                                case hint of
                                    Just hintText ->
                                        Html.div
                                            [ Attributes.class "hint" ]
                                            [ Html.text hintText ]

                                    Nothing ->
                                        Html.text ""
                        ]
                )

-}
customizeInput :
    ({ isRequired : Bool
     , label : List (Attribute msg) -> Html msg
     , input : List (Attribute msg) -> Html msg
     , hint : Maybe String
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
        , idString : String
        }
     }
     -> Html msg
    )
    -> View id val msg
    -> View id val msg
customizeInput viewFunc (View input path params) =
    View input path { params | inputView = viewFunc }


{-| TODO
-}
customizeGroup :
    ({ legend : List (Attribute msg) -> Html msg
     , inputs : List (Html msg)
     , identifier : Maybe id
     }
     -> Html msg
    )
    -> View id val msg
    -> View id val msg
customizeGroup viewFunc (View input path params) =
    View input path { params | groupView = viewFunc }


{-| TODO
-}
customizeRepeatableInputsGroup :
    ({ legend : List (Attribute msg) -> Html msg
     , inputs : List (Html msg)
     , addInputButton : List (Attribute msg) -> Html msg
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


{-| TODO
-}
customizeRepeatableInput :
    ({ input : Html msg
     , removeInputButton : List (Attribute msg) -> Html msg
     , advanced :
        { identifier : Maybe id
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


{-| TODO
-}
toHtmlHelp : ViewAttributes id val msg -> List Int -> Input id val -> Html msg
toHtmlHelp attributes path ((Input tree) as input) =
    let
        unwrappedInput =
            Tree.value tree

        wrapInput inputHtml =
            attributes.inputView
                { isRequired = unwrappedInput.isRequired
                , label = toAttrs >> labelToHtml unwrappedInput.label path input
                , input = toAttrs >> inputHtml
                , errors =
                    inputErrors input
                        |> List.map
                            (\error ->
                                attributes.errorToString
                                    { error = error
                                    , inputType = mapInputType unwrappedInput.inputType
                                    }
                            )
                , hint = unwrappedInput.hint
                , advanced =
                    { identifier = unwrappedInput.identifier
                    , inputType = mapInputType unwrappedInput.inputType
                    , inputName = unwrappedInput.name
                    , inputPlaceholder = unwrappedInput.placeholder
                    , inputValue = Value.Value unwrappedInput.value
                    , inputMin = Value.Value unwrappedInput.min
                    , inputMax = Value.Value unwrappedInput.max
                    , inputOptions = List.map (Tuple.mapSecond Value.Value) unwrappedInput.options
                    , inputOnChange =
                        \(Value.Value val) ->
                            InputChanged path val |> attributes.onChange
                    , labelText = unwrappedInput.label
                    , idString = inputId input path
                    }
                }
    in
    case unwrappedInput.inputType of
        Internal.Group ->
            groupToHtml attributes path input

        Internal.Repeatable _ ->
            repeatableToHtml attributes path input

        Internal.Text ->
            wrapInput (inputToHtml attributes "text" path input [])

        Internal.Email ->
            wrapInput (inputToHtml attributes "email" path input [])

        Internal.Password ->
            wrapInput (inputToHtml attributes "password" path input [])

        Internal.TextArea ->
            wrapInput (textAreaToHtml attributes path input)

        Internal.Integer ->
            inputToHtml attributes "number" path input [ Attributes.step "1" ]
                |> wrapInput

        Internal.Float ->
            inputToHtml attributes "number" path input [ Attributes.step "1" ]
                |> wrapInput

        Internal.Date ->
            wrapInput (inputToHtml attributes "date" path input [])

        Internal.Month ->
            wrapInput (inputToHtml attributes "month" path input [])

        Internal.Select ->
            wrapInput (selectToHtml attributes path input)

        Internal.Radio ->
            wrapInput (radioToHtml attributes path input)

        Internal.Checkbox ->
            wrapInput (checkboxToHtml attributes path input)


labelToHtml : Maybe String -> List Int -> Input id val -> (UserAttributes -> Html msg)
labelToHtml label path input element =
    case label of
        Just str ->
            Html.label
                (Attributes.for (inputId input path) :: userProvidedAttributes element)
                [ Html.text str ]

        Nothing ->
            Html.text ""


groupToHtml : ViewAttributes id val msg -> List Int -> Input id val -> Html msg
groupToHtml attributes path ((Input tree) as input) =
    let
        { identifier } =
            Tree.value tree
    in
    attributes.groupView
        { legend = toAttrs >> legendToHtml input
        , inputs =
            Tree.children tree
                |> List.indexedMap
                    (\idx ->
                        Input >> toHtmlHelp attributes (path ++ [ idx ])
                    )
        , identifier = identifier
        }


legendToHtml : Input id val -> UserAttributes -> Html msg
legendToHtml (Input tree) element =
    case Tree.value tree |> .label of
        Just labelStr ->
            Html.legend (userProvidedAttributes element) [ Html.text labelStr ]

        Nothing ->
            Html.text ""


repeatableToHtml : ViewAttributes id val msg -> List Int -> Input id val -> Html msg
repeatableToHtml attributes path ((Input tree) as input) =
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
                { input = toHtmlHelp attributes childPath (Input child)
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
        { legend = toAttrs >> legendToHtml input
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
inputToHtml { onChange } inputType path (Input tree) htmlAttrs element =
    let
        unwrappedInput =
            Tree.value tree
    in
    Html.input
        (List.concat
            [ htmlAttrs
            , Attributes.type_ inputType
                :: valueAttribute Attributes.value unwrappedInput.value
                :: onInputChanged onChange (Input tree) path
                :: inputAttrs onChange path (Input tree)
            , userProvidedAttributes element
            ]
        )
        []


textAreaToHtml :
    ViewAttributes id val msg
    -> List Int
    -> Input id val
    -> (UserAttributes -> Html msg)
textAreaToHtml { onChange } path (Input input) element =
    let
        valueStr =
            Tree.value input
                |> .value
                |> Internal.Value.toString
                |> Result.withDefault ""
    in
    Html.div
        [ Attributes.class "grow-wrap"
        , Attributes.attribute "data-replicated-value" valueStr
        ]
        [ Html.textarea
            (List.concat
                [ onInputChanged onChange (Input input) path
                    :: Attributes.value valueStr
                    :: inputAttrs onChange path (Input input)
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
selectToHtml { onChange } path ((Input tree) as input) element =
    let
        unwappedInput =
            Tree.value tree
    in
    Html.select
        (Attributes.id (inputId input path)
            :: Attributes.required unwappedInput.isRequired
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
radioToHtml { onChange } path (Input input) element =
    let
        unwrappedInput =
            Tree.value input
    in
    Html.div
        [ Attributes.class "radios" ]
        (List.indexedMap
            (\index ( optionText, optionValue ) ->
                let
                    optionId =
                        identifierString
                            (Maybe.map ((++) (String.fromInt index)) unwrappedInput.name)
                            path
                in
                Html.div
                    []
                    [ Html.input
                        (Attributes.id optionId
                            :: Attributes.checked (optionValue == unwrappedInput.value)
                            :: Attributes.required unwrappedInput.isRequired
                            :: Attributes.value (String.fromInt index)
                            :: Attributes.type_ "radio"
                            :: onInputChanged onChange (Input input) path
                            :: onInputFocused onChange path
                            :: onInputBlured onChange path
                            :: userProvidedAttributes element
                        )
                        []
                    , Html.label
                        [ Attributes.for optionId ]
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
checkboxToHtml { onChange } path (Input input) element =
    Html.input
        (List.concat
            [ Attributes.type_ "checkbox"
                :: (Tree.value input
                        |> .value
                        |> Internal.Value.toBool
                        |> Result.map Attributes.checked
                        |> Result.withDefault (Attributes.class "")
                   )
                :: Events.onCheck
                    (Internal.Value.fromBool
                        >> InputChanged path
                        >> onChange
                    )
                :: inputAttrs onChange path (Input input)
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
        |> Result.map f
        |> Result.withDefault (Attributes.class "")


inputAttrs : (Msg id val -> msg) -> List Int -> Input id val -> List (Html.Attribute msg)
inputAttrs attrs path ((Input tree) as input) =
    let
        unwrappedInput =
            Tree.value tree
    in
    [ Attributes.id (inputId input path)
    , Attributes.required unwrappedInput.isRequired
    , Attributes.autocomplete False
    , Attributes.placeholder (Maybe.withDefault "" unwrappedInput.placeholder)
    , onInputFocused attrs path
    , onInputBlured attrs path
    , valueAttribute Attributes.min unwrappedInput.min
    , valueAttribute Attributes.max unwrappedInput.max
    ]


onInputChanged : (Msg id val -> c) -> Input id val -> List Int -> Html.Attribute c
onInputChanged tagger input path =
    Events.onInput (Internal.inputChanged input path >> tagger)


onInputFocused : (Msg id val -> msg) -> List Int -> Html.Attribute msg
onInputFocused tagger path =
    Events.onFocus (tagger (InputFocused path))


onInputBlured : (Msg id val -> msg) -> List Int -> Html.Attribute msg
onInputBlured tagger path =
    Events.onBlur (tagger (InputBlured path))


inputId : Input id val -> List Int -> String
inputId (Input input) path =
    identifierString (Tree.value input |> .name) path


identifierString : Maybe String -> List Int -> String
identifierString maybe path =
    case maybe of
        Just str ->
            String.join "-" (str :: List.map String.fromInt path)

        Nothing ->
            String.join "-" (List.map String.fromInt path)


inputErrors : Input id val -> List (Error id val)
inputErrors (Input tree) =
    let
        { errors, status } =
            Tree.value tree
    in
    case status of
        Touched ->
            errors

        _ ->
            []


groupView : GroupView id msg -> Html msg
groupView view =
    Html.fieldset [] (view.legend [] :: view.inputs)


repeatableInputsGroupView : RepeatableInputsGroupView id msg -> Html msg
repeatableInputsGroupView params =
    Html.fieldset
        []
        [ Html.div [] (params.legend [] :: params.inputs)
        , params.addInputButton []
        ]


repeatableInputView : RepeatableInputView id msg -> Html msg
repeatableInputView params =
    Html.div
        [ Attributes.class "group-repeat" ]
        [ params.input
        , params.removeInputButton []
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
            err :: _ ->
                Html.p [ Attributes.class "error" ] [ Html.text err ]

            [] ->
                case hint of
                    Just hintText ->
                        Html.div
                            [ Attributes.class "hint" ]
                            [ Html.text hintText ]

                    Nothing ->
                        Html.text ""
        ]


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


mapInputType : Internal.InputType id val err -> InputType
mapInputType inputType =
    case inputType of
        Internal.Text ->
            Text

        Internal.TextArea ->
            TextArea

        Internal.Password ->
            Password

        Internal.Email ->
            Email

        Internal.Integer ->
            Integer

        Internal.Float ->
            Float

        Internal.Month ->
            Month

        Internal.Date ->
            Date

        Internal.Select ->
            Select

        Internal.Radio ->
            Radio

        Internal.Checkbox ->
            Checkbox

        Internal.Repeatable _ ->
            Text

        Internal.Group ->
            Text


toAttrs : List (Attribute msg) -> UserAttributes
toAttrs =
    List.foldl (\(Attribute f) attrs -> f attrs) { classList = [], styles = [] }


{-| -}
class : String -> Attribute msg
class classStr =
    classList [ ( classStr, True ) ]


{-| -}
classList : List ( String, Bool ) -> Attribute msg
classList classTuple =
    Attribute
        (\attrs ->
            { attrs | classList = classTuple ++ attrs.classList }
        )


{-| -}
style : String -> String -> Attribute msg
style key val =
    Attribute (\attrs -> { attrs | styles = ( key, val ) :: attrs.styles })


userProvidedAttributes : UserAttributes -> List (Html.Attribute msg)
userProvidedAttributes element =
    Attributes.classList element.classList
        :: List.map (\( k, v ) -> Attributes.style k v) element.styles


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

        CustomError _ message ->
            message

        _ ->
            "Couldn't parse"
