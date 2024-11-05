module Internal.View exposing
    ( View, init, partial, toHtml
    , UserAttributes, defaultAttributes
    , inputId
    )

{-|

@docs View, init, partial, toHtml
@docs UserAttributes, defaultAttributes
@docs inputId

-}

import FormToolkit.Decode as Decode exposing (Error(..))
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.Input as Input exposing (Input, Status(..))
import Internal.Value
import Json.Decode
import RoseTree.Tree as Tree


type alias Input id val =
    Input.Input id val (Error id val)


type alias View id val msg =
    { attributes : ViewAttributes id val msg
    , path : List Int
    , input : Input id val
    }


type alias ViewAttributes id val msg =
    { onChange : List Int -> Internal.Value.Value val -> msg
    , onFocus : List Int -> msg
    , onBlur : List Int -> msg
    , onAdd : List Int -> msg
    , onRemove : List Int -> msg
    , errorToString : Error id val -> String
    , inputView : InputView msg -> Html msg
    , groupView : GroupView id msg -> Html msg
    , repeatableInputsGroupView : RepeatableInputsGroupView msg -> Html msg
    , repeatableInputView : RepeatableInputView msg -> Html msg
    }


type alias InputView msg =
    { isRequired : Bool
    , label : UserAttributes -> Html msg
    , input : UserAttributes -> Html msg
    , hint : UserAttributes -> Html msg
    , errors : List String
    , path : List Int
    }


type alias GroupView id msg =
    { legendText : Maybe String
    , inputs : List (Html msg)
    , identifier : Maybe id
    , errors : List String
    }


type alias RepeatableInputsGroupView msg =
    { legendText : Maybe String
    , inputs : List (Html msg)
    , addInputButton : UserAttributes -> Html msg
    , addInputButtonOnClick : Maybe msg
    , errors : List String
    , path : List Int
    }


type alias RepeatableInputView msg =
    { input : Html msg
    , removeInputButton : UserAttributes -> Html msg
    , index : Int
    , removeInputButtonCopy : String
    , removeInputButtonOnClick : Maybe msg
    }


init :
    { events :
        { onChange : List Int -> Internal.Value.Value val -> msg
        , onFocus : List Int -> msg
        , onBlur : List Int -> msg
        , onAdd : List Int -> msg
        , onRemove : List Int -> msg
        }
    , path : List Int
    , input : Input id val
    }
    -> View id val msg
init { events, path, input } =
    { attributes =
        { onChange = events.onChange
        , onFocus = events.onFocus
        , onBlur = events.onBlur
        , onAdd = events.onAdd
        , onRemove = events.onRemove
        , errorToString = errorToString
        , inputView = inputView
        , groupView = groupView
        , repeatableInputsGroupView = repeatableInputsGroupView
        , repeatableInputView = repeatableInputView
        }
    , path = path
    , input =
        Decode.validateAndDecode (Decode.succeed ()) input
            |> Tuple.first
    }


partial : id -> View id val msg -> Maybe (View id val msg)
partial id { input, attributes } =
    findNode id input
        |> Maybe.map (\( found, path ) -> View attributes path found)


findNode : id -> Input id val -> Maybe ( Input id val, List Int )
findNode id =
    Tree.foldWithPath
        (\path node foundPath ->
            if .identifier (Tree.value node) == Just id then
                Just ( node, path )

            else
                foundPath
        )
        Nothing


toHtml : View id val msg -> Html msg
toHtml { input, path, attributes } =
    let
        unwrappedInput =
            Tree.value input

        wrapInput : (UserAttributes -> Html msg) -> Html msg
        wrapInput inputHtml =
            attributes.inputView
                { isRequired = unwrappedInput.isRequired
                , label = labelToHtml unwrappedInput.label path input
                , input = inputHtml
                , errors = visibleErrors input |> List.map attributes.errorToString
                , hint =
                    \attrList ->
                        case unwrappedInput.hint of
                            Just hintText ->
                                Html.div
                                    (Attributes.class "hint"
                                        :: Attributes.id (hintId input path)
                                        :: userProvidedAttributes attrList
                                    )
                                    [ Html.text hintText ]

                            Nothing ->
                                Html.text ""
                , path = path
                }
    in
    case unwrappedInput.inputType of
        Input.Group ->
            groupToHtml attributes path input

        Input.Repeatable _ ->
            repeatableToHtml attributes path input

        Input.Text ->
            wrapInput (inputToHtml attributes "text" path input [])

        Input.StrictAutocomplete ->
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
groupToHtml attributes path input =
    let
        { identifier, label } =
            Tree.value input
    in
    attributes.groupView
        { legendText = label
        , inputs =
            Tree.children input
                |> List.indexedMap
                    (\idx -> View attributes (path ++ [ idx ]) >> toHtml)
        , identifier = identifier
        , errors = visibleErrors input |> List.map attributes.errorToString
        }


repeatableToHtml : ViewAttributes id val msg -> List Int -> Input id val -> Html msg
repeatableToHtml attributes path input =
    let
        unwrappedInput =
            Tree.value input

        children =
            Tree.children input

        childrenCount =
            List.length children

        inputsView idx child =
            let
                childPath =
                    path ++ [ idx ]

                removeInputButtonOnClick =
                    attributes.onRemove childPath

                removeInputButtonCopy =
                    Tree.value child |> .removeInputsButtonCopy

                removeInputButtonEnabled =
                    childrenCount > unwrappedInput.repeatableMin
            in
            attributes.repeatableInputView
                { input = toHtml (View attributes childPath child)
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
                                :: userProvidedAttributes attrList
                            )
                            [ Html.text removeInputButtonCopy
                            ]
                , index = idx
                , removeInputButtonCopy = removeInputButtonCopy
                , removeInputButtonOnClick =
                    if removeInputButtonEnabled then
                        Just removeInputButtonOnClick

                    else
                        Nothing
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
                                ( attributes.onAdd path
                                , True
                                )
                            )
                        :: userProvidedAttributes attrList
                    )
                    [ Html.text unwrappedInput.addInputsButtonCopy ]
        , addInputButtonOnClick =
            if addInputButtonEnabled then
                Just (attributes.onAdd path)

            else
                Nothing
        , errors = visibleErrors input |> List.map attributes.errorToString
        , path = path
        }


inputToHtml :
    ViewAttributes id val msg
    -> String
    -> List Int
    -> Input id val
    -> List (Html.Attribute msg)
    -> (UserAttributes -> Html msg)
inputToHtml attributes inputType path input htmlAttrs element =
    let
        unwrappedInput =
            Tree.value input

        inputHtml =
            Html.input
                (List.concat
                    [ htmlAttrs
                    , Attributes.type_ inputType
                        :: valueAttribute Attributes.value unwrappedInput.value
                        :: Events.onInput
                            (attributes.onChange path << Input.strToValue input)
                        :: textInputHtmlAttributes attributes path input
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
textAreaToHtml attributes path input element =
    let
        { value, autogrow } =
            Tree.value input

        valueStr =
            value
                |> Internal.Value.toString
                |> Maybe.withDefault ""

        autogrowAttrs =
            if autogrow then
                [ Attributes.style "resize" "none"
                , Attributes.style "overflow" "hidden"
                , Attributes.style "grid-area" "1/1/2/2"
                , Attributes.style "font" "inherit"
                ]

            else
                []
    in
    Html.div
        [ if autogrow then
            Attributes.style "display" "grid"

          else
            Attributes.class ""
        ]
        (Html.textarea
            (List.concat
                [ Events.onInput (attributes.onChange path << Input.strToValue input)
                    :: Attributes.value valueStr
                    :: textInputHtmlAttributes attributes path input
                , userProvidedAttributes element
                , autogrowAttrs
                ]
            )
            []
            :: (if autogrow then
                    [ Html.div
                        (List.concat
                            [ Attributes.attribute "aria-hidden" "true"
                                :: Attributes.style "white-space" "pre-wrap"
                                :: Attributes.style "visibility" "hidden"
                                :: userProvidedAttributes element
                            , autogrowAttrs
                            ]
                        )
                        [ Html.text (valueStr ++ "\n") ]
                    ]

                else
                    []
               )
        )


selectToHtml :
    ViewAttributes id val msg
    -> List Int
    -> Input id val
    -> (UserAttributes -> Html msg)
selectToHtml { onChange, onFocus, onBlur } path input element =
    let
        unwappedInput =
            Tree.value input
    in
    Html.select
        (Attributes.id (inputId input path)
            :: Attributes.required unwappedInput.isRequired
            :: Events.onInput (onChange path << Input.strToValue input)
            :: Events.onFocus (onFocus path)
            :: Events.onBlur (onBlur path)
            :: nameAttribute input
            :: ariaLabeledByAttribute input path
            :: ariaDescribedByAttribute input path
            :: ariaInvalidAttribute input
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
radioToHtml { onChange, onFocus, onBlur } path input element =
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
                            :: Events.onInput (onChange path << Input.strToValue input)
                            :: Events.onFocus (onFocus path)
                            :: Events.onBlur (onBlur path)
                            :: nameAttribute input
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
checkboxToHtml attributes path input element =
    Html.input
        (List.concat
            [ Attributes.type_ "checkbox"
                :: (Tree.value input
                        |> .value
                        |> Internal.Value.toBool
                        |> Maybe.map Attributes.checked
                        |> Maybe.withDefault (Attributes.class "")
                   )
                :: Events.onCheck (Internal.Value.fromBool >> attributes.onChange path)
                :: textInputHtmlAttributes attributes path input
            , userProvidedAttributes element
            ]
        )
        []


valueAttribute :
    (String -> Html.Attribute msg)
    -> Internal.Value.Value val
    -> Html.Attribute msg
valueAttribute f inputValue =
    case inputValue of
        Internal.Value.Invalid ->
            f ""

        _ ->
            Internal.Value.toString inputValue
                |> Maybe.map f
                |> Maybe.withDefault (Attributes.class "")


textInputHtmlAttributes : ViewAttributes id val msg -> List Int -> Input id val -> List (Html.Attribute msg)
textInputHtmlAttributes attributes path input =
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
          , Events.onFocus (attributes.onFocus path)
          , Events.onBlur (attributes.onBlur path)
          , nameAttribute input
          , valueAttribute Attributes.min (Input.min input)
          , valueAttribute Attributes.max (Input.max input)
          , ariaDescribedByAttribute input path
          , ariaInvalidAttribute input
          ]
        ]


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


visibleErrors : Input id val -> List (Error id val)
visibleErrors input =
    let
        { errors, status, inputType } =
            Tree.value input
    in
    case ( status, inputType ) of
        ( Touched, _ ) ->
            errors

        ( _, Input.Repeatable _ ) ->
            errors

        ( _, Input.Group ) ->
            errors

        _ ->
            errors
                |> List.filter
                    (\err ->
                        case err of
                            NoOptionsProvided _ ->
                                True

                            _ ->
                                False
                    )


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


repeatableInputsGroupView : RepeatableInputsGroupView msg -> Html msg
repeatableInputsGroupView { legendText, addInputButton, inputs, errors } =
    Html.fieldset []
        [ case legendText of
            Just str ->
                Html.legend [] [ Html.text str ]

            Nothing ->
                Html.text ""
        , Html.div [] inputs
        , addInputButton defaultAttributes
        , viewErrors errors
        ]


repeatableInputView : RepeatableInputView msg -> Html msg
repeatableInputView { input, removeInputButton } =
    Html.div
        [ Attributes.class "group-repeat" ]
        [ input
        , removeInputButton defaultAttributes
        ]


inputView : InputView msg -> Html msg
inputView { isRequired, label, input, errors, hint } =
    Html.div
        [ Attributes.class "field"
        , Attributes.classList [ ( "required", isRequired ) ]
        ]
        [ label defaultAttributes
        , Html.div
            [ Attributes.class "input-wrapper" ]
            [ input defaultAttributes ]
        , case errors of
            [] ->
                hint defaultAttributes

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


type alias UserAttributes =
    { classList : List ( String, Bool )
    , styles : List ( String, String )
    }


defaultAttributes : UserAttributes
defaultAttributes =
    { classList = []
    , styles = []
    }


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
        |> Maybe.map
            (\_ -> Attributes.attribute "aria-describedby" (hintId input path))
        |> Maybe.withDefault (Attributes.class "")


ariaLabeledByAttribute : Input id val -> List Int -> Html.Attribute msg
ariaLabeledByAttribute input path =
    Input.label input
        |> Maybe.map
            (\_ -> Attributes.attribute "aria-labelledby" (labelId input path))
        |> Maybe.withDefault (Attributes.class "")


ariaInvalidAttribute : Input id val -> Html.Attribute msg
ariaInvalidAttribute input =
    if List.isEmpty (visibleErrors input) then
        Attributes.class ""

    else
        Attributes.attribute "aria-invalid" "true"


errorToString : Error id val -> String
errorToString error =
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

        NoOptionsProvided _ ->
            "No options have been provided"

        CustomError _ message ->
            message

        _ ->
            "Couldn't parse"
