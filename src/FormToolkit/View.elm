module FormToolkit.View exposing
    ( View, fromInput, toHtml
    , partial
    , InputType(..), Attribute, class, classList, style
    , customizeError, customizeInput
    , customizeGroup, customizeRepeatableGroup, customizeRepeatableItem
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
@docs customizeGroup, customizeRepeatableGroup, customizeRepeatableItem

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


type alias Msg id =
    Internal.Msg id


type alias ViewAttributes id val msg =
    { onChange : Msg id -> msg
    , errorsList : Error id val -> String
    , inputView : InputView id val msg -> Html msg
    , groupView : GroupView id msg -> Html msg
    , repeatableGroupView : RepeatableGroupView id msg -> Html msg
    , repeatableItemView : RepeatableItemView id msg -> Html msg
    }


type alias InputView id val msg =
    { isRequired : Bool
    , identifier : Maybe id
    , inputType : InputType
    , inputValue : Value val
    , label : List (Attribute msg) -> Html msg
    , input : List (Attribute msg) -> Html msg
    , hint : Maybe String
    , errors : List String
    }


type alias GroupView id msg =
    { identifier : Maybe id
    , legend : List (Attribute msg) -> Html msg
    , inputs : List (Html msg)
    }


type alias RepeatableGroupView id msg =
    { identifier : Maybe id
    , legend : List (Attribute msg) -> Html msg
    , inputs : List (Html msg)
    , addInputButton : List (Attribute msg) -> Html msg
    , addInputButtonOnClick : Maybe msg
    , addInputButtonCopy : String
    }


type alias RepeatableItemView id msg =
    { identifier : Maybe id
    , inputs : Html msg
    , removeInputButton : List (Attribute msg) -> Html msg
    , removeInputButtonOnClick : Maybe msg
    , removeInputButtonCopy : String
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
fromInput : (Msg id -> msg) -> Input id val -> View id val msg
fromInput onChange input =
    View input
        []
        { onChange = onChange
        , errorsList = errorsToString
        , groupView = groupView
        , repeatableGroupView = repeatableGroupView
        , repeatableItemView = repeatableItemView
        , inputView = inputView
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


{-| TODO
-}
customizeInput :
    ({ isRequired : Bool
     , identifier : Maybe id
     , inputType : InputType
     , inputValue : Value val
     , label : List (Attribute msg) -> Html msg
     , input : List (Attribute msg) -> Html msg
     , hint : Maybe String
     , errors : List String
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
    ({ identifier : Maybe id
     , legend : List (Attribute msg) -> Html msg
     , inputs : List (Html msg)
     }
     -> Html msg
    )
    -> View id val msg
    -> View id val msg
customizeGroup viewFunc (View input path params) =
    View input path { params | groupView = viewFunc }


{-| TODO
-}
customizeRepeatableGroup :
    ({ identifier : Maybe id
     , legend : List (Attribute msg) -> Html msg
     , inputs : List (Html msg)
     , addInputButton : List (Attribute msg) -> Html msg
     , addInputButtonOnClick : Maybe msg
     , addInputButtonCopy : String
     }
     -> Html msg
    )
    -> View id val msg
    -> View id val msg
customizeRepeatableGroup viewFunc (View input path params) =
    View input path { params | repeatableGroupView = viewFunc }


{-| TODO
-}
customizeRepeatableItem :
    ({ identifier : Maybe id
     , inputs : Html msg
     , removeInputButton : List (Attribute msg) -> Html msg
     , removeInputButtonOnClick : Maybe msg
     , removeInputButtonCopy : String
     }
     -> Html msg
    )
    -> View id val msg
    -> View id val msg
customizeRepeatableItem viewFunc (View input path params) =
    View input path { params | repeatableItemView = viewFunc }


{-| TODO
-}
customizeError : (Error id val -> String) -> View id val msg -> View id val msg
customizeError viewFunc (View input path params) =
    View input path { params | errorsList = viewFunc }


{-| TODO
-}
toHtmlHelp : ViewAttributes id val msg -> List Int -> Input id val -> Html msg
toHtmlHelp attributes path ((Input tree) as node) =
    let
        { hint, label, isRequired, identifier, inputType, value } =
            Tree.value tree

        wrapInput inputHtml =
            attributes.inputView
                { isRequired = isRequired
                , identifier = identifier
                , inputType = mapInputType inputType
                , inputValue = Value.Value value
                , label = toAttrs >> labelToHtml label path node
                , input = toAttrs >> inputHtml
                , errors = inputErrors node |> List.map attributes.errorsList
                , hint = hint
                }
    in
    case inputType of
        Internal.Group ->
            groupToHtml attributes path node

        Internal.Repeatable _ ->
            repeatableToHtml attributes path node

        Internal.Text ->
            wrapInput (inputToHtml attributes "text" path node [])

        Internal.Email ->
            wrapInput (inputToHtml attributes "email" path node [])

        Internal.Password ->
            wrapInput (inputToHtml attributes "password" path node [])

        Internal.TextArea ->
            wrapInput (textAreaToHtml attributes path node)

        Internal.Integer ->
            inputToHtml attributes "number" path node [ Attributes.step "1" ]
                |> wrapInput

        Internal.Float ->
            inputToHtml attributes "number" path node [ Attributes.step "1" ]
                |> wrapInput

        Internal.Date ->
            wrapInput (inputToHtml attributes "date" path node [])

        Internal.Month ->
            wrapInput (inputToHtml attributes "month" path node [])

        Internal.Select ->
            wrapInput (selectToHtml attributes path node)

        Internal.Radio ->
            wrapInput (radioToHtml attributes path node)

        Internal.Checkbox ->
            wrapInput (checkboxToHtml attributes path node)


labelToHtml : Maybe String -> List Int -> Input id val -> (Element -> Html msg)
labelToHtml label path node element =
    case label of
        Just str ->
            Html.label
                (Attributes.for (inputId node path) :: userProvidedAttributes element)
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
        { identifier = identifier
        , legend = toAttrs >> legendToHtml input
        , inputs =
            Tree.children tree
                |> List.indexedMap
                    (\idx ->
                        Input >> toHtmlHelp attributes (path ++ [ idx ])
                    )
        }


legendToHtml : Input id val -> Element -> Html msg
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
                    Tree.value child |> .removeInputsText

                removeInputButtonEnabled =
                    childrenCount > unwrappedInput.repeatableMin
            in
            attributes.repeatableItemView
                { identifier = Tree.value tree |> .identifier
                , inputs = toHtmlHelp attributes childPath (Input child)
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
                , removeInputButtonOnClick =
                    if removeInputButtonEnabled then
                        Just removeInputButtonOnClick

                    else
                        Nothing
                , removeInputButtonCopy = removeInputButtonCopy
                }

        addInputButtonEnabled =
            case unwrappedInput.repeatableMax of
                Just max ->
                    childrenCount < max

                Nothing ->
                    True
    in
    repeatableGroupView
        { identifier = unwrappedInput.identifier
        , legend = toAttrs >> legendToHtml input
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
                    [ Html.text unwrappedInput.addInputsText ]
        , addInputButtonOnClick =
            if addInputButtonEnabled then
                Just (attributes.onChange (InputsAdded path))

            else
                Nothing
        , addInputButtonCopy = unwrappedInput.addInputsText
        }


inputToHtml :
    ViewAttributes id val msg
    -> String
    -> List Int
    -> Input id val
    -> List (Html.Attribute msg)
    -> (Element -> Html msg)
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
                :: onInputChanged onChange path
                :: inputAttrs onChange path (Input tree)
            , userProvidedAttributes element
            ]
        )
        []


textAreaToHtml :
    ViewAttributes id val msg
    -> List Int
    -> Input id val
    -> (Element -> Html msg)
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
                [ onInputChanged onChange path
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
    -> (Element -> Html msg)
selectToHtml { onChange } path ((Input tree) as input) element =
    let
        unwappedInput =
            Tree.value tree
    in
    Html.select
        (Attributes.id (inputId input path)
            :: Attributes.required unwappedInput.isRequired
            :: onInputChanged onChange path
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
    -> (Element -> Html msg)
radioToHtml { onChange } path (Input tree) element =
    let
        unwrappedInput =
            Tree.value tree
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
                            :: onInputChanged onChange path
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
    -> (Element -> Html msg)
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
                :: Events.onCheck (InputChecked path >> onChange)
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


inputAttrs : (Msg id -> msg) -> List Int -> Input id val -> List (Html.Attribute msg)
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


onInputChanged : (Msg id -> msg) -> List Int -> Html.Attribute msg
onInputChanged tagger path =
    Events.onInput (InputChanged path >> tagger)


onInputFocused : (Msg id -> msg) -> List Int -> Html.Attribute msg
onInputFocused tagger path =
    Events.onFocus (tagger (InputFocused path))


onInputBlured : (Msg id -> msg) -> List Int -> Html.Attribute msg
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


repeatableGroupView : RepeatableGroupView id msg -> Html msg
repeatableGroupView params =
    Html.fieldset
        []
        [ Html.div [] (params.legend [] :: params.inputs)
        , params.addInputButton []
        ]


repeatableItemView : RepeatableItemView id msg -> Html msg
repeatableItemView params =
    Html.div
        [ Attributes.class "group-repeat" ]
        [ params.inputs
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
    = Attribute (Element -> Element)


type alias Element =
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


toAttrs : List (Attribute msg) -> Element
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


userProvidedAttributes : Element -> List (Html.Attribute msg)
userProvidedAttributes element =
    Attributes.classList element.classList
        :: List.map (\( k, v ) -> Attributes.style k v) element.styles


errorsToString : Error id val -> String
errorsToString err =
    let
        toString =
            Value.toString >> Maybe.withDefault ""
    in
    case err of
        ValueTooLarge _ data ->
            "Should be lesser than " ++ toString data.max

        ValueTooSmall _ data ->
            "Should be greater than " ++ toString data.min

        ValueNotInRange _ data ->
            "Should be between " ++ toString data.min ++ " and " ++ toString data.max

        IsBlank _ ->
            "Should be provided"

        CustomError message ->
            message

        _ ->
            "Couldn't parse"
