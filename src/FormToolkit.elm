module FormToolkit exposing
    ( Msg, update
    , View, initView
    , toHtml
    , withInputView, withErrorsView
    , withGroupView, withRepeatableView, withTemplateView
    )

{-|


# Update

@docs Msg, update


# View

@docs View, initView
@docs toHtml


# View customizations

@docs withInputView, withErrorsView
@docs withGroupView, withRepeatableView, withTemplateView

-}

import FormToolkit.Decode exposing (Decoder)
import FormToolkit.Input exposing (Attribute(..), Error(..), Input(..))
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.Input as Internal exposing (Status(..))
import Internal.Value
import Json.Decode
import RoseTree.Tree as Tree


{-| TODO
-}
type Msg id
    = InputChanged (List Int) String
    | InputChecked (List Int) Bool
    | InputFocused (List Int)
    | InputBlured (List Int)
    | InputsAdded (List Int)
    | InputsRemoved (List Int)


type alias Tree id =
    Tree.Tree (Internal.Input id (Error id))


{-| TODO
-}
update :
    Msg id
    -> Decoder id a
    -> Input id
    -> ( Input id, Result (List (Error id)) a )
update msg decoder input =
    FormToolkit.Decode.validateAndDecode decoder <|
        case msg of
            InputChanged path str ->
                updateAt path (updateInput str) input

            InputChecked path bool ->
                updateAt path (updateInputWithBool bool) input

            InputFocused path ->
                updateAt path (Tree.updateValue Internal.focus) input

            InputBlured path ->
                updateAt path (Tree.updateValue Internal.blur) input

            InputsAdded path ->
                case
                    Tree.getValueAt path (toTree input)
                        |> Maybe.map .inputType
                of
                    Just (Internal.Repeatable template) ->
                        updateAt path (Tree.push template) input

                    _ ->
                        input

            InputsRemoved path ->
                Input (Tree.removeAt path (toTree input))


updateInput : String -> Tree id -> Tree id
updateInput string =
    Tree.updateValue (Internal.updateWithString string)


updateInputWithBool : Bool -> Tree id -> Tree id
updateInputWithBool bool =
    Tree.updateValue (Internal.update (Internal.Value.fromBool bool))


updateAt : List Int -> (Tree id -> Tree id) -> Input id -> Input id
updateAt path func input =
    Input (Tree.updateAt path func (toTree input))


type alias ViewAttributes id msg =
    { onChange : Msg id -> msg
    , errorsView : Error id -> Html msg
    , groupView : GroupView msg -> Html msg
    , repeatableView : RepeatableView msg -> Html msg
    , templateView : TemplateView msg -> Html msg
    , inputView : InputView msg -> Html msg
    }


{-| TODO
-}
type View id msg
    = View (Input id) (ViewAttributes id msg)


{-| TODO
-}
initView : (Msg id -> msg) -> Input id -> View id msg
initView onChange input =
    View input
        { onChange = onChange
        , errorsView = errorsView
        , groupView = groupView
        , repeatableView = repeatableView
        , templateView = templateView
        , inputView = inputView
        }


{-| TODO
-}
withInputView :
    ({ isRequired : Bool
     , labelHtml : Html msg
     , inputHtml : Html msg
     , errorsHtml : List (Html msg)
     , hint : Maybe String
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
withInputView viewFunc (View input params) =
    View input { params | inputView = viewFunc }


{-| TODO
-}
withErrorsView : (Error id -> Html msg) -> View id msg -> View id msg
withErrorsView viewFunc (View input params) =
    View input { params | errorsView = viewFunc }


{-| TODO
-}
withGroupView :
    ({ inline : Bool, legendHtml : Html msg, inputsHtml : List (Html msg) }
     -> Html msg
    )
    -> View id msg
    -> View id msg
withGroupView viewFunc (View input params) =
    View input { params | groupView = viewFunc }


{-| TODO
-}
withRepeatableView :
    ({ onAddAttribute : Html.Attribute msg
     , legendHtml : Html msg
     , inputsHtml : List (Html msg)
     , addButtonText : String
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
withRepeatableView viewFunc (View input params) =
    View input { params | repeatableView = viewFunc }


{-| TODO
-}
withTemplateView :
    ({ onRemoveAttribute : Html.Attribute msg
     , removeButtonText : String
     , showRemoveButton : Bool
     , inputsHtml : Html msg
     }
     -> Html msg
    )
    -> View id msg
    -> View id msg
withTemplateView viewFunc (View input params) =
    View input { params | templateView = viewFunc }


{-| TODO
-}
toHtml : View id msg -> Html msg
toHtml (View input attributes) =
    toHtmlHelp attributes [] input


{-| TODO
-}
toHtmlHelp : ViewAttributes id msg -> List Int -> Input id -> Html msg
toHtmlHelp attributes path node =
    case Tree.value (toTree node) |> .inputType of
        Internal.Group ->
            groupToHtml attributes path node

        Internal.Repeatable _ ->
            repeatableToHtml attributes path node

        Internal.Text ->
            inputToHtml attributes "text" path node []
                |> wrapInput attributes path node

        Internal.Email ->
            inputToHtml attributes "email" path node []
                |> wrapInput attributes path node

        Internal.Password ->
            inputToHtml attributes "password" path node []
                |> wrapInput attributes path node

        Internal.TextArea ->
            textAreaToHtml attributes path node
                |> wrapInput attributes path node

        Internal.Integer ->
            inputToHtml attributes "number" path node [ Attributes.step "1" ]
                |> wrapInput attributes path node

        Internal.Float ->
            inputToHtml attributes "number" path node [ Attributes.step "1" ]
                |> wrapInput attributes path node

        Internal.Date ->
            inputToHtml attributes "date" path node []
                |> wrapInput attributes path node

        Internal.Month ->
            inputToHtml attributes "month" path node []
                |> wrapInput attributes path node

        Internal.Select ->
            selectToHtml attributes path node
                |> wrapInput attributes path node

        Internal.Radio ->
            radioToHtml attributes path node
                |> wrapInput attributes path node

        Internal.Checkbox ->
            checkboxToHtml attributes path node
                |> wrapInput attributes path node


groupToHtml : ViewAttributes id msg -> List Int -> Input id -> Html msg
groupToHtml attributes path node =
    let
        tree =
            toTree node

        input =
            Tree.value tree
    in
    attributes.groupView
        { inline = input.inline
        , legendHtml =
            case input.label of
                Just labelStr ->
                    Html.legend [] [ Html.text labelStr ]

                Nothing ->
                    Html.text ""
        , inputsHtml =
            Tree.children tree
                |> List.indexedMap
                    (\idx ->
                        Input >> toHtmlHelp attributes (path ++ [ idx ])
                    )
        }


repeatableToHtml : ViewAttributes id msg -> List Int -> Input id -> Html msg
repeatableToHtml attributes path node =
    let
        tree =
            toTree node

        input =
            Tree.value tree

        children =
            Tree.children tree
    in
    repeatableView
        { onAddAttribute =
            Events.preventDefaultOn "click"
                (Json.Decode.succeed
                    ( attributes.onChange (InputsAdded path), True )
                )
        , legendHtml = legend input.label
        , inputsHtml =
            children
                |> List.indexedMap
                    (\idx ->
                        Input
                            >> templateHtml attributes
                                (path ++ [ idx ])
                                (List.length children /= (idx + 1))
                    )
        , addButtonText = "Add"
        }


inputToHtml :
    ViewAttributes id msg
    -> String
    -> List Int
    -> Input id
    -> List (Html.Attribute msg)
    -> Html msg
inputToHtml { onChange } inputType path input htmlAttrs =
    let
        actualInput =
            Tree.value (toTree input)
    in
    Html.input
        (htmlAttrs
            ++ Attributes.type_ inputType
            :: valueAttribute Attributes.value actualInput.value
            :: onInputChanged onChange path
            :: inputAttrs onChange path input
        )
        []


textAreaToHtml : ViewAttributes id msg -> List Int -> Input id -> Html msg
textAreaToHtml { onChange } path (Input input) =
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
                :: inputAttrs onChange path (Input input)
            )
            []
        ]


selectToHtml : ViewAttributes id msg -> List Int -> Input id -> Html msg
selectToHtml { onChange } path input =
    let
        data =
            Tree.value (toTree input)
    in
    Html.select
        [ Attributes.id (inputId input path)
        , Attributes.required data.isRequired
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


radioToHtml : ViewAttributes id msg -> List Int -> Input id -> Html msg
radioToHtml { onChange } path input =
    let
        data =
            Tree.value (toTree input)
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


checkboxToHtml : ViewAttributes id msg -> List Int -> Input id -> Html msg
checkboxToHtml { onChange } path (Input input) =
    Html.input
        (Attributes.type_ "checkbox"
            :: (Tree.value input
                    |> .value
                    |> Internal.Value.toBool
                    |> Result.map Attributes.checked
                    |> Result.withDefault (Attributes.class "")
               )
            :: Events.onCheck (InputChecked path >> onChange)
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
inputAttrs attrs path input =
    let
        data =
            Tree.value (toTree input)
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


wrapInput : ViewAttributes id msg -> List Int -> Input id -> Html msg -> Html msg
wrapInput attrs path input inputHtml =
    let
        { hint, label, isRequired } =
            Tree.value (toTree input)
    in
    inputView
        { isRequired = isRequired
        , labelHtml =
            case label of
                Just str ->
                    Html.label
                        [ Attributes.for (inputId input path) ]
                        [ Html.text str
                        ]

                Nothing ->
                    Html.text ""
        , inputHtml = inputHtml
        , errorsHtml = inputErrors input |> List.map attrs.errorsView
        , hint = hint
        }


templateHtml : ViewAttributes id msg -> List Int -> Bool -> Input id -> Html msg
templateHtml attributes path isLast node =
    attributes.templateView
        { onRemoveAttribute =
            Events.preventDefaultOn "click"
                (Json.Decode.succeed
                    ( attributes.onChange (InputsRemoved path), True )
                )
        , removeButtonText = "Remove"
        , showRemoveButton = isLast
        , inputsHtml = toHtmlHelp attributes path node
        }


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
inputErrors input =
    let
        { errors, status } =
            Tree.value (toTree input)
    in
    case status of
        Touched ->
            errors

        _ ->
            []


{-| -}
toTree : Input id -> Tree id
toTree (Input tree) =
    tree


type alias GroupView msg =
    { inline : Bool
    , legendHtml : Html msg
    , inputsHtml : List (Html msg)
    }


groupView : GroupView msg -> Html msg
groupView { inline, legendHtml, inputsHtml } =
    Html.fieldset
        [ Attributes.classList
            [ ( "inline", inline )
            , ( "stacked", not inline )
            ]
        ]
        (legendHtml :: inputsHtml)


type alias RepeatableView msg =
    { onAddAttribute : Html.Attribute msg
    , legendHtml : Html msg
    , inputsHtml : List (Html msg)
    , addButtonText : String
    }


repeatableView : RepeatableView msg -> Html msg
repeatableView { onAddAttribute, addButtonText, legendHtml, inputsHtml } =
    Html.fieldset
        []
        [ Html.div [] (legendHtml :: inputsHtml)
        , Html.button
            [ Attributes.class "add-fields"
            , onAddAttribute
            ]
            [ Html.text addButtonText ]
        ]


type alias TemplateView msg =
    { onRemoveAttribute : Html.Attribute msg
    , removeButtonText : String
    , showRemoveButton : Bool
    , inputsHtml : Html msg
    }


templateView : TemplateView msg -> Html msg
templateView { onRemoveAttribute, removeButtonText, showRemoveButton, inputsHtml } =
    Html.div
        [ Attributes.class "group-repeat" ]
        [ inputsHtml
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


type alias InputView msg =
    { isRequired : Bool
    , labelHtml : Html msg
    , inputHtml : Html msg
    , errorsHtml : List (Html msg)
    , hint : Maybe String
    }


inputView : InputView msg -> Html msg
inputView { isRequired, labelHtml, inputHtml, errorsHtml, hint } =
    Html.div
        [ Attributes.class "field"
        , Attributes.classList [ ( "required", isRequired ) ]
        ]
        [ labelHtml
        , Html.div [ Attributes.class "input-wrapper" ] [ inputHtml ]
        , case errorsHtml of
            errorHtml :: _ ->
                Html.p [ Attributes.class "error" ] [ errorHtml ]

            [] ->
                case hint of
                    Just hintText ->
                        Html.div
                            [ Attributes.class "hint" ]
                            [ Html.text hintText ]

                    Nothing ->
                        Html.text ""
        ]


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

            _ ->
                "Couldn't parse"
        )
