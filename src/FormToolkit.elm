module FormToolkit exposing
    ( Msg, update
    , ViewAttribute, toHtml, onChange
    , addInputsButtonContent, removeInputsButtonContent
    , elementHtml
    )

{-|


# Update

@docs Msg, update


# View

@docs ViewAttribute, toHtml, onChange
@docs addInputsButtonContent, removeInputsButtonContent
@docs elementHtml

-}

-- import Json.Encode as Encode

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
type ViewAttribute id msg
    = ViewAttribute (ViewAttributes id msg -> ViewAttributes id msg)


{-| TODO
-}
type alias ViewAttributes id msg =
    { onChange : Maybe (Msg id -> msg)
    , addInputsButtonContent : Maybe id -> Html msg
    , removeInputsButtonContent : Maybe id -> Html msg
    , elements : List ( id, Html msg )
    }


{-| TODO
-}
toHtml : List (ViewAttribute id msg) -> Input id -> Html msg
toHtml attributes =
    toHtmlHelp
        (List.foldl (\(ViewAttribute func) params -> func params)
            { onChange = Nothing
            , addInputsButtonContent = \_ -> Html.text "Add"
            , removeInputsButtonContent = \_ -> Html.text "Remove"
            , elements = []
            }
            attributes
        )
        []


{-| TODO
-}
onChange : (Msg id -> msg) -> ViewAttribute id msg
onChange tagger =
    ViewAttribute (\attrs -> { attrs | onChange = Just tagger })


{-| TODO
-}
addInputsButtonContent : (Maybe id -> Html msg) -> ViewAttribute id msg
addInputsButtonContent func =
    ViewAttribute (\attrs -> { attrs | addInputsButtonContent = func })


{-| TODO
-}
removeInputsButtonContent : (Maybe id -> Html msg) -> ViewAttribute id msg
removeInputsButtonContent func =
    ViewAttribute (\attrs -> { attrs | removeInputsButtonContent = func })


{-| TODO
-}
elementHtml : id -> Html msg -> ViewAttribute id msg
elementHtml id html =
    ViewAttribute
        (\attrs ->
            { attrs | elements = ( id, html ) :: attrs.elements }
        )


{-| TODO
-}
toHtmlHelp : ViewAttributes id msg -> List Int -> Input id -> Html msg
toHtmlHelp attrs path node =
    let
        tree =
            toTree node

        input =
            Tree.value tree
    in
    case input.inputType of
        Internal.Group ->
            let
                children =
                    Tree.children tree
                        |> List.indexedMap
                            (\idx ->
                                Input >> toHtmlHelp attrs (path ++ [ idx ])
                            )

                inputLabel =
                    case input.label of
                        Just labelStr ->
                            Html.legend [] [ Html.text labelStr ]

                        Nothing ->
                            Html.text ""
            in
            Html.fieldset
                [ Attributes.id (inputId node path)
                , Attributes.classList
                    [ ( "inline", input.inline )
                    , ( "stacked", not input.inline )
                    ]
                ]
                (inputLabel :: children)

        Internal.Repeatable data ->
            let
                children =
                    Tree.children tree

                inputs =
                    children
                        |> List.indexedMap
                            (\idx ->
                                Input
                                    >> templateHtml attrs
                                        (path ++ [ idx ])
                                        (List.length children /= (idx + 1))
                            )
            in
            Html.fieldset
                [ Attributes.id (inputId node path) ]
                [ Html.div [] (legend input.label :: inputs)
                , addInputsButton attrs (Tree.value data |> .identifier) path
                ]

        Internal.Text ->
            inputToHtml attrs "text" path node []
                |> wrapInput path node

        Internal.Email ->
            inputToHtml attrs "email" path node []
                |> wrapInput path node

        Internal.Password ->
            inputToHtml attrs "password" path node []
                |> wrapInput path node

        Internal.TextArea ->
            textAreaToHtml attrs path node
                |> wrapInput path node

        Internal.Integer ->
            inputToHtml attrs "number" path node [ Attributes.step "1" ]
                |> wrapInput path node

        Internal.Float ->
            inputToHtml attrs "number" path node [ Attributes.step "1" ]
                |> wrapInput path node

        Internal.Date ->
            inputToHtml attrs "date" path node []
                |> wrapInput path node

        Internal.Month ->
            inputToHtml attrs "month" path node []
                |> wrapInput path node

        Internal.Select ->
            selectToHtml attrs path node
                |> wrapInput path node

        Internal.Radio ->
            radioToHtml attrs path node
                |> wrapInput path node

        Internal.Checkbox ->
            checkboxToHtml attrs path node
                |> wrapInput path node

        Internal.Element elementId ->
            attrs.elements
                |> List.filter (Tuple.first >> (==) elementId)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault (Html.text "")


inputToHtml :
    ViewAttributes id msg
    -> String
    -> List Int
    -> Input id
    -> List (Html.Attribute msg)
    -> Html msg
inputToHtml attrs inputType path input htmlAttrs =
    let
        actualInput =
            Tree.value (toTree input)
    in
    Html.input
        (htmlAttrs
            ++ Attributes.type_ inputType
            :: valueAttribute Attributes.value actualInput.value
            :: onInputChanged attrs path
            :: inputAttrs attrs path input
        )
        []


textAreaToHtml : ViewAttributes id msg -> List Int -> Input id -> Html msg
textAreaToHtml attrs path (Input input) =
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
            (onInputChanged attrs path
                :: Attributes.value valueStr
                :: inputAttrs attrs path (Input input)
            )
            []
        ]


selectToHtml : ViewAttributes id msg -> List Int -> Input id -> Html msg
selectToHtml attrs path input =
    let
        data =
            Tree.value (toTree input)
    in
    Html.select
        [ Attributes.id (inputId input path)
        , Attributes.required data.isRequired
        , onInputChanged attrs path
        , onInputFocused attrs path
        , onInputBlured attrs path
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
radioToHtml attrs path input =
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
                        , onInputChanged attrs path
                        , onInputFocused attrs path
                        , onInputBlured attrs path
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
checkboxToHtml attrs path (Input input) =
    Html.input
        (Attributes.type_ "checkbox"
            :: (Tree.value input
                    |> .value
                    |> Internal.Value.toBool
                    |> Result.map Attributes.checked
                    |> Result.withDefault (Attributes.class "")
               )
            :: (case attrs.onChange of
                    Just tagger ->
                        Events.onCheck (InputChecked path >> tagger)

                    Nothing ->
                        Attributes.class ""
               )
            :: inputAttrs attrs path (Input input)
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


inputAttrs :
    ViewAttributes id msg
    -> List Int
    -> Input id
    -> List (Html.Attribute msg)
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


onInputChanged : ViewAttributes id msg -> List Int -> Html.Attribute msg
onInputChanged attrs path =
    case attrs.onChange of
        Just tagger ->
            Events.onInput (InputChanged path >> tagger)

        Nothing ->
            Attributes.class ""


onInputFocused : ViewAttributes id msg -> List Int -> Html.Attribute msg
onInputFocused attrs path =
    case attrs.onChange of
        Just tagger ->
            Events.onFocus (tagger (InputFocused path))

        Nothing ->
            Attributes.class ""


onInputBlured : ViewAttributes id msg -> List Int -> Html.Attribute msg
onInputBlured attrs path =
    case attrs.onChange of
        Just tagger ->
            Events.onBlur (tagger (InputBlured path))

        Nothing ->
            Attributes.class ""


wrapInput : List Int -> Input id -> Html msg -> Html msg
wrapInput path input inputHtml =
    let
        { hint, label, isRequired } =
            Tree.value (toTree input)
    in
    fieldView
        { isRequired = isRequired
        , label =
            case label of
                Just str ->
                    Html.label
                        [ Attributes.for (inputId input path) ]
                        [ Html.text str
                        ]

                Nothing ->
                    Html.text ""
        , field = inputHtml
        , errors = inputErrors input
        , hint = hint
        }


fieldView :
    { isRequired : Bool
    , label : Html msg
    , field : Html msg
    , errors : List (Error id)
    , hint : Maybe String
    }
    -> Html msg
fieldView { isRequired, label, field, errors, hint } =
    Html.div
        [ Attributes.class "field"
        , Attributes.classList [ ( "required", isRequired ) ]
        ]
        [ label
        , Html.div [ Attributes.class "input-wrapper" ] [ field ]
        , case errors of
            message :: _ ->
                Html.p [ Attributes.class "error" ] [ errorToHtml message ]

            [] ->
                case hint of
                    Just hintText ->
                        Html.div
                            [ Attributes.class "hint" ]
                            [ Html.text hintText ]

                    Nothing ->
                        Html.text ""
        ]


templateHtml :
    ViewAttributes id msg
    -> List Int
    -> Bool
    -> Input id
    -> Html msg
templateHtml attributes path isLast inputElement =
    Html.div
        [ Attributes.class "group-repeat" ]
        [ toHtmlHelp attributes path inputElement
        , if isLast then
            Html.button
                [ Attributes.class "remove-fields"
                , case attributes.onChange of
                    Just tagger ->
                        Events.preventDefaultOn "click"
                            (Json.Decode.succeed
                                ( tagger (InputsRemoved path), True )
                            )

                    Nothing ->
                        Attributes.class ""
                ]
                [ attributes.removeInputsButtonContent (toId inputElement)
                ]

          else
            Html.text ""
        ]


addInputsButton : ViewAttributes id msg -> Maybe id -> List Int -> Html msg
addInputsButton attrs id path =
    Html.button
        [ Attributes.class "add-fields"
        , case attrs.onChange of
            Just tagger ->
                Events.preventDefaultOn "click"
                    (Json.Decode.succeed ( tagger (InputsAdded path), True ))

            Nothing ->
                Attributes.class ""
        ]
        [ attrs.addInputsButtonContent id
        ]


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
type Msg id
    = InputChanged (List Int) String
    | InputChecked (List Int) Bool
    | InputFocused (List Int)
    | InputBlured (List Int)
    | InputsAdded (List Int)
    | InputsRemoved (List Int)


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


updateInput :
    String
    -> Tree.Tree (Internal.Input id (Error id))
    -> Tree.Tree (Internal.Input id (Error id))
updateInput string =
    Tree.updateValue (Internal.updateWithString string)


updateInputWithBool :
    Bool
    -> Tree.Tree (Internal.Input id (Error id))
    -> Tree.Tree (Internal.Input id (Error id))
updateInputWithBool bool =
    Tree.updateValue (Internal.update (Internal.Value.fromBool bool))


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
toTree : Input id -> Tree.Tree (Internal.Input id (Error id))
toTree (Input tree) =
    tree


toId : Input id -> Maybe id
toId (Input input) =
    Tree.value input |> .identifier


updateAt :
    List Int
    -> (Tree.Tree (Internal.Input id (Error id)) -> Tree.Tree (Internal.Input id (Error id)))
    -> Input id
    -> Input id
updateAt path func input =
    Input (Tree.updateAt path func (toTree input))


{-| TODO
-}
errorToHtml : Error id -> Html msg
errorToHtml err =
    let
        toString =
            Value.toString
                >> Maybe.withDefault ""
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
