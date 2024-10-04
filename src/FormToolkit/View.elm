module FormToolkit.View exposing
    ( View, fromInput, toHtml
    , partial
    , InputType(..), Attribute, class, classList
    , customizeInput
    , customizeGroup, customizeRepeatable, customizeTemplate
    , customizeErrors
    )

{-|


# View

@docs View, fromInput, toHtml
@docs partial


# View customizations

@docs InputType, Attribute, class, classList
@docs customizeInput
@docs customizeGroup, customizeRepeatable, customizeTemplate
@docs customizeErrors

-}

import FormToolkit.Decode exposing (Error(..))
import FormToolkit.Value as Value
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


type alias Tree id =
    Internal.Tree id (Error id)


type alias Input id =
    Internal.Input id (Error id)


type alias Msg id =
    Internal.Msg id


type alias ViewAttributes id msg =
    { onChange : Msg id -> msg
    , errorsView : Error id -> Html msg
    , groupView : GroupView msg -> Html msg
    , repeatableView : RepeatableView msg -> Html msg
    , templateView : TemplateView msg -> Html msg
    , inputView : InputView id msg -> Html msg
    }


type alias InputView id msg =
    { isRequired : Bool
    , identifier : ( InputType, Maybe id )
    , label : List (Attribute msg) -> Html msg
    , input : List (Attribute msg) -> Html msg
    , errors : List (Html msg)
    , hint : Maybe String
    }


{-| Represents a view for an [Input](FormToolkit.Input#Input) for the purposes
of customized rendering.
-}
type View id msg
    = View (Input id) (List Int) (ViewAttributes id msg)


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
fromInput : (Msg id -> msg) -> Input id -> View id msg
fromInput onChange input =
    View input
        []
        { onChange = onChange
        , errorsView = errorsView
        , groupView = groupView
        , repeatableView = repeatableView
        , templateView = templateView
        , inputView = inputView
        }


{-| Render a view
-}
toHtml : View id msg -> Html msg
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
partial : id -> View id msg -> Maybe (View id msg)
partial id (View input _ attributes) =
    findNode id input
        |> Maybe.map (\( found, path ) -> View found path attributes)


findNode : id -> Input id -> Maybe ( Input id, List Int )
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
     , identifier : ( InputType, Maybe id )
     , label : List (Attribute msg) -> Html msg
     , input : List (Attribute msg) -> Html msg
     , errors : List (Html msg)
     , hint : Maybe String
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
customizeInput viewFunc (View input path params) =
    View input path { params | inputView = viewFunc }


{-| TODO
-}
customizeGroup :
    ({ inline : Bool
     , legend : List (Attribute msg) -> Html msg
     , inputs : List (Html msg)
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
customizeGroup viewFunc (View input path params) =
    View input path { params | groupView = viewFunc }


{-| TODO
-}
customizeRepeatable :
    ({ legend : Html msg
     , inputs : List (Html msg)
     , onAddAttribute : Html.Attribute msg
     , showAddButton : Bool
     , addButtonText : String
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
customizeRepeatable viewFunc (View input path params) =
    View input path { params | repeatableView = viewFunc }


{-| TODO
-}
customizeTemplate :
    ({ onRemoveAttribute : Html.Attribute msg
     , removeButtonText : String
     , showRemoveButton : Bool
     , inputs : Html msg
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
customizeTemplate viewFunc (View input path params) =
    View input path { params | templateView = viewFunc }


{-| TODO
-}
customizeErrors : (Error id -> Html msg) -> View id msg -> View id msg
customizeErrors viewFunc (View input path params) =
    View input path { params | errorsView = viewFunc }


{-| TODO
-}
toHtmlHelp : ViewAttributes id msg -> List Int -> Input id -> Html msg
toHtmlHelp attributes path ((Input tree) as node) =
    let
        value =
            Tree.value tree

        wrapInput inputHtml =
            let
                { hint, label, isRequired } =
                    Tree.value tree
            in
            attributes.inputView
                { isRequired = isRequired
                , identifier =
                    ( mapInputType value.inputType
                    , value.identifier
                    )
                , label = toAttrs >> labelToHtml label path node
                , input = toAttrs >> inputHtml
                , errors = inputErrors node |> List.map attributes.errorsView
                , hint = hint
                }
    in
    case value.inputType of
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


labelToHtml : Maybe String -> List Int -> Input id -> (Element -> Html msg)
labelToHtml label path node element =
    case label of
        Just str ->
            Html.label
                [ Attributes.for (inputId node path)
                , Attributes.classList element.classList
                ]
                [ Html.text str
                ]

        Nothing ->
            Html.text ""


groupToHtml : ViewAttributes id msg -> List Int -> Input id -> Html msg
groupToHtml attributes path ((Input tree) as input) =
    attributes.groupView
        { inline = Tree.value tree |> .inline
        , legend = toAttrs >> legendToHtml input
        , inputs =
            Tree.children tree
                |> List.indexedMap
                    (\idx ->
                        Input >> toHtmlHelp attributes (path ++ [ idx ])
                    )
        }


legendToHtml : Input id -> Element -> Html msg
legendToHtml (Input tree) element =
    case Tree.value tree |> .label of
        Just labelStr ->
            Html.legend
                [ Attributes.classList element.classList ]
                [ Html.text labelStr ]

        Nothing ->
            Html.text ""


repeatableToHtml : ViewAttributes id msg -> List Int -> Input id -> Html msg
repeatableToHtml attributes path (Input tree) =
    let
        input =
            Tree.value tree

        children =
            Tree.children tree

        childrenCount =
            List.length children

        inputsView idx child =
            let
                childPath =
                    path ++ [ idx ]
            in
            attributes.templateView
                { onRemoveAttribute =
                    Events.preventDefaultOn "click"
                        (Json.Decode.succeed
                            ( attributes.onChange (InputsRemoved childPath)
                            , True
                            )
                        )
                , removeButtonText = Tree.value child |> .removeInputsText
                , showRemoveButton = childrenCount > input.repeatableMin
                , inputs = toHtmlHelp attributes childPath (Input child)
                }
    in
    repeatableView
        { legend = legend input.label
        , inputs = children |> List.indexedMap inputsView
        , showAddButton =
            case input.repeatableMax of
                Just max ->
                    childrenCount < max

                Nothing ->
                    True
        , onAddAttribute =
            Events.preventDefaultOn "click"
                (Json.Decode.succeed
                    ( attributes.onChange (InputsAdded path), True )
                )
        , addButtonText = input.addInputsText
        }


inputToHtml :
    ViewAttributes id msg
    -> String
    -> List Int
    -> Input id
    -> List (Html.Attribute msg)
    -> (Element -> Html msg)
inputToHtml { onChange } inputType path (Input tree) htmlAttrs element =
    let
        input =
            Tree.value tree
    in
    Html.input
        (htmlAttrs
            ++ Attributes.type_ inputType
            :: valueAttribute Attributes.value input.value
            :: onInputChanged onChange path
            :: Attributes.classList element.classList
            :: inputAttrs onChange path (Input tree)
        )
        []


textAreaToHtml :
    ViewAttributes id msg
    -> List Int
    -> Input id
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
            (onInputChanged onChange path
                :: Attributes.value valueStr
                :: Attributes.classList element.classList
                :: inputAttrs onChange path (Input input)
            )
            []
        ]


selectToHtml :
    ViewAttributes id msg
    -> List Int
    -> Input id
    -> (Element -> Html msg)
selectToHtml { onChange } path ((Input tree) as input) element =
    let
        data =
            Tree.value tree
    in
    Html.select
        [ Attributes.id (inputId input path)
        , Attributes.required data.isRequired
        , Attributes.classList element.classList
        , onInputChanged onChange path
        , onInputFocused onChange path
        , onInputBlured onChange path
        ]
        (Html.option [] []
            :: List.indexedMap
                (\index ( optionText, optionValue ) ->
                    Html.option
                        [ Attributes.selected (optionValue == data.value)
                        , Attributes.value (String.fromInt index)
                        ]
                        [ Html.text optionText ]
                )
                data.options
        )


radioToHtml :
    ViewAttributes id msg
    -> List Int
    -> Input id
    -> (Element -> Html msg)
radioToHtml { onChange } path (Input tree) element =
    let
        data =
            Tree.value tree
    in
    Html.div
        [ Attributes.class "radios" ]
        (List.indexedMap
            (\index ( optionText, optionValue ) ->
                let
                    optionId =
                        identifierString
                            (Maybe.map ((++) (String.fromInt index)) data.name)
                            path
                in
                Html.div
                    []
                    [ Html.input
                        [ Attributes.id optionId
                        , Attributes.checked (optionValue == data.value)
                        , Attributes.required data.isRequired
                        , Attributes.value (String.fromInt index)
                        , Attributes.type_ "radio"
                        , Attributes.classList element.classList
                        , onInputChanged onChange path
                        , onInputFocused onChange path
                        , onInputBlured onChange path
                        ]
                        []
                    , Html.label
                        [ Attributes.for optionId ]
                        [ Html.text optionText ]
                    ]
            )
            data.options
        )


checkboxToHtml :
    ViewAttributes id msg
    -> List Int
    -> Input id
    -> (Element -> Html msg)
checkboxToHtml { onChange } path (Input input) element =
    Html.input
        (Attributes.type_ "checkbox"
            :: (Tree.value input
                    |> .value
                    |> Internal.Value.toBool
                    |> Result.map Attributes.checked
                    |> Result.withDefault (Attributes.class "")
               )
            :: Events.onCheck (InputChecked path >> onChange)
            :: Attributes.classList element.classList
            :: inputAttrs onChange path (Input input)
        )
        []


valueAttribute :
    (String -> Html.Attribute msg)
    -> Internal.Value.Value
    -> Html.Attribute msg
valueAttribute f inputValue =
    Internal.Value.toString inputValue
        |> Result.map f
        |> Result.withDefault (Attributes.class "")


inputAttrs : (Msg id -> msg) -> List Int -> Input id -> List (Html.Attribute msg)
inputAttrs attrs path ((Input tree) as input) =
    let
        data =
            Tree.value tree
    in
    [ Attributes.id (inputId input path)
    , Attributes.required data.isRequired
    , Attributes.autocomplete False
    , Attributes.placeholder (Maybe.withDefault "" data.placeholder)
    , onInputFocused attrs path
    , onInputBlured attrs path
    , valueAttribute Attributes.min data.min
    , valueAttribute Attributes.max data.max
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


inputId : Input id -> List Int -> String
inputId (Input input) path =
    identifierString (Tree.value input |> .name) path


identifierString : Maybe String -> List Int -> String
identifierString maybe path =
    case maybe of
        Just str ->
            String.join "-" (str :: List.map String.fromInt path)

        Nothing ->
            String.join "-" (List.map String.fromInt path)


legend : Maybe String -> Html msg
legend =
    Maybe.map (\t -> Html.legend [] [ Html.text t ])
        >> Maybe.withDefault (Html.text "")


{-| TODO
-}
inputErrors : Input id -> List (Error id)
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


type alias GroupView msg =
    { inline : Bool
    , legend : List (Attribute msg) -> Html msg
    , inputs : List (Html msg)
    }


groupView : GroupView msg -> Html msg
groupView view =
    Html.fieldset
        [ Attributes.classList
            [ ( "inline", view.inline )
            , ( "stacked", not view.inline )
            ]
        ]
        (view.legend [] :: view.inputs)


type alias RepeatableView msg =
    { legend : Html msg
    , inputs : List (Html msg)
    , onAddAttribute : Html.Attribute msg
    , showAddButton : Bool
    , addButtonText : String
    }


repeatableView : RepeatableView msg -> Html msg
repeatableView attrs =
    Html.fieldset
        []
        [ Html.div [] (attrs.legend :: attrs.inputs)
        , if attrs.showAddButton then
            Html.button
                [ Attributes.class "add-fields"
                , attrs.onAddAttribute
                ]
                [ Html.text attrs.addButtonText ]

          else
            Html.text ""
        ]


type alias TemplateView msg =
    { onRemoveAttribute : Html.Attribute msg
    , removeButtonText : String
    , showRemoveButton : Bool
    , inputs : Html msg
    }


templateView : TemplateView msg -> Html msg
templateView { onRemoveAttribute, removeButtonText, showRemoveButton, inputs } =
    Html.div
        [ Attributes.class "group-repeat" ]
        [ inputs
        , if showRemoveButton then
            Html.button
                [ Attributes.class "remove-fields"
                , onRemoveAttribute
                ]
                [ Html.text removeButtonText
                ]

          else
            Html.text ""
        ]


inputView : InputView id msg -> Html msg
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
            html :: _ ->
                Html.p [ Attributes.class "error" ] [ html ]

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


mapInputType : Internal.InputType id err -> InputType
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
    List.foldl (\(Attribute f) attrs -> f attrs) { classList = [] }


{-| TODO
-}
class : String -> Attribute msg
class classStr =
    classList ( classStr, True )


{-| TODO
-}
classList : ( String, Bool ) -> Attribute msg
classList classTuple =
    Attribute (\attrs -> { attrs | classList = classTuple :: attrs.classList })


errorsView : Error id -> Html msg
errorsView err =
    let
        toString =
            Value.toString >> Maybe.withDefault ""
    in
    Html.text
        (case err of
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
        )
