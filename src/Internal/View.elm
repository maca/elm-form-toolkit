module Internal.View exposing
    ( View, init, partial, toHtml
    )

{-|

@docs View, init, partial, toHtml

-}

import Array
import Dict
import FormToolkit.Error exposing (Error(..))
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.Field exposing (FieldType(..), Status(..))
import Internal.Value
import Json.Decode
import Json.Encode
import RoseTree.Tree as Tree


type alias Node id =
    Tree.Tree (Attributes id)


type alias Attributes id =
    Internal.Field.Attributes id (FieldType id (List (Error id))) (List (Error id))


type alias View id msg =
    { onChange : Maybe id -> List Int -> Internal.Value.Value -> { selectionStart : Int, selectionEnd : Int } -> msg
    , onCheck : Maybe id -> List Int -> Bool -> msg
    , onFocus : Maybe id -> List Int -> msg
    , onBlur : Maybe id -> List Int -> msg
    , onAdd : Maybe id -> List Int -> msg
    , onRemove : Maybe id -> List Int -> msg
    , errorToString : Attributes id -> Error id -> String
    , fieldView : FieldView id msg -> Html msg
    , checkboxFieldView : FieldView id msg -> Html msg
    , groupView : GroupView id msg -> Html msg
    , repeatableFieldsGroupView : RepeatableFieldsGroupView id msg -> Html msg
    , repeatableFieldView : RepeatableFieldView id msg -> Html msg
    , path : List Int
    , root : Node id
    }


type alias FieldView id msg =
    { labelHtml : List (Html.Attribute msg) -> Html msg
    , inputHtml : List (Html.Attribute msg) -> Html msg
    , hintHtml : List (Html.Attribute msg) -> Html msg
    , errors : List String
    , path : List Int
    , class : String
    , attributes : Attributes id
    }


type alias GroupView id msg =
    { legendText : Maybe String
    , fields : List (Html msg)
    , identifier : Maybe id
    , errors : List String
    , class : String
    }


type alias RepeatableFieldsGroupView id msg =
    { legendText : Maybe String
    , fields : List (Html msg)
    , addFieldsButton : List (Html.Attribute msg) -> Html msg
    , addFieldsButtonOnClick : Maybe msg
    , errors : List String
    , path : List Int
    , class : String
    , attributes : Attributes id
    }


type alias RepeatableFieldView id msg =
    { field : Html msg
    , removeFieldsButton : List (Html.Attribute msg) -> Html msg
    , index : Int
    , removeFieldsButtonCopy : String
    , removeFieldsButtonOnClick : Maybe msg
    , class : String
    , attributes : Attributes id
    }


init :
    { onChange : Maybe id -> List Int -> Internal.Value.Value -> { selectionStart : Int, selectionEnd : Int } -> msg
    , onCheck : Maybe id -> List Int -> Bool -> msg
    , onFocus : Maybe id -> List Int -> msg
    , onBlur : Maybe id -> List Int -> msg
    , onAdd : Maybe id -> List Int -> msg
    , onRemove : Maybe id -> List Int -> msg
    , path : List Int
    , field : Node id
    }
    -> View id msg
init attrs =
    { onChange = attrs.onChange
    , onCheck = attrs.onCheck
    , onFocus = attrs.onFocus
    , onBlur = attrs.onBlur
    , onAdd = attrs.onAdd
    , onRemove = attrs.onRemove
    , errorToString = \_ -> FormToolkit.Error.toEnglish
    , fieldView = fieldView
    , checkboxFieldView = checkboxFieldView
    , groupView = groupView
    , repeatableFieldsGroupView = repeatableFieldsGroupView
    , repeatableFieldView = repeatableFieldView
    , path = attrs.path
    , root = attrs.field
    }


partial : id -> View id msg -> Maybe (View id msg)
partial id view =
    findNode id view.root
        |> Maybe.map
            (\( found, path ) -> { view | path = path, root = found })


findNode : id -> Node id -> Maybe ( Node id, List Int )
findNode id =
    Tree.foldWithPath
        (\path node foundPath ->
            if .identifier (Tree.value node) == Just id then
                Just ( node, path )

            else
                foundPath
        )
        Nothing


inputStringToValue : Node id -> String -> Internal.Value.Value
inputStringToValue input str =
    let
        unwrappedField =
            Tree.value input

        getChoice () =
            case String.toInt str of
                Just idx ->
                    Array.fromList unwrappedField.options
                        |> Array.get idx
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault Internal.Value.blank

                Nothing ->
                    Internal.Value.blank
    in
    case unwrappedField.inputType of
        Text ->
            Internal.Value.fromNonBlankString str

        TextArea ->
            Internal.Value.fromNonEmptyString str

        Password ->
            Internal.Value.fromNonBlankString str

        StrictAutocomplete ->
            Dict.fromList unwrappedField.options
                |> Dict.get str
                |> Maybe.withDefault Internal.Value.blank

        Email ->
            Internal.Value.fromNonBlankString str

        Integer ->
            Internal.Value.intFromString str

        Float ->
            Internal.Value.floatFromString str

        Month ->
            Internal.Value.monthFromString str

        Date ->
            Internal.Value.dateFromString str

        LocalDatetime ->
            Internal.Value.timeFromString str

        Select ->
            getChoice ()

        Radio ->
            getChoice ()

        Checkbox ->
            case str of
                "true" ->
                    Internal.Value.fromBool True

                "false" ->
                    Internal.Value.fromBool False

                _ ->
                    Internal.Value.blank

        Group ->
            Internal.Value.blank

        Repeatable _ ->
            Internal.Value.blank


toHtml : View id msg -> Html msg
toHtml view =
    let
        attrs =
            Tree.value view.root

        wrapInput : (List (Html.Attribute msg) -> Html msg) -> Html msg
        wrapInput inputHtml =
            view.fieldView
                { labelHtml = labelToHtml attrs.label view.path view.root
                , inputHtml = inputHtml
                , errors = visibleErrors view.root |> List.map (view.errorToString attrs)
                , hintHtml =
                    \attrList ->
                        case attrs.hint of
                            Just hintText ->
                                Html.div
                                    (Attributes.class "hint"
                                        :: Attributes.id (hintId view.root view.path)
                                        :: attrList
                                    )
                                    [ Html.text hintText ]

                            Nothing ->
                                Html.text ""
                , path = view.path
                , class = String.join " " attrs.classList
                , attributes = attrs
                }
    in
    case ( attrs.hidden, attrs.inputType ) of
        ( True, _ ) ->
            Html.text ""

        ( False, Group ) ->
            groupToHtml view

        ( False, Repeatable _ ) ->
            repeatableToHtml view

        ( False, Text ) ->
            wrapInput (inputToHtml view "text" [])

        ( False, StrictAutocomplete ) ->
            wrapInput (inputToHtml view "text" [])

        ( False, Email ) ->
            wrapInput (inputToHtml view "email" [])

        ( False, Password ) ->
            wrapInput (inputToHtml view "password" [])

        ( False, TextArea ) ->
            wrapInput (textAreaToHtml view)

        ( False, Integer ) ->
            inputToHtml view "number" [ valueAttribute Attributes.step (Tree.value view.root).step ]
                |> wrapInput

        ( False, Float ) ->
            inputToHtml view "number" [ valueAttribute Attributes.step (Tree.value view.root).step ]
                |> wrapInput

        ( False, Date ) ->
            wrapInput (inputToHtml view "date" [])

        ( False, Month ) ->
            wrapInput (inputToHtml view "month" [])

        ( False, LocalDatetime ) ->
            wrapInput (inputToHtml view "datetime-local" [])

        ( False, Select ) ->
            wrapInput (selectToHtml view)

        ( False, Radio ) ->
            wrapInput (radioToHtml view)

        ( False, Checkbox ) ->
            checkboxToHtml view


labelToHtml : Maybe String -> List Int -> Node id -> (List (Html.Attribute msg) -> Html msg)
labelToHtml label path input element =
    case label of
        Just str ->
            Html.label
                (Attributes.for (inputId input path)
                    :: Attributes.id (labelId input path)
                    :: (if (Tree.value input).inputType == Checkbox then
                            Attributes.class "label-inline" :: element

                        else
                            element
                       )
                )
                [ Html.text str ]

        Nothing ->
            Html.text ""


groupToHtml : View id msg -> Html msg
groupToHtml view =
    let
        ({ identifier, label, classList } as attrs) =
            Tree.value view.root
    in
    view.groupView
        { legendText = label
        , fields =
            Tree.children view.root
                |> List.indexedMap
                    (\idx child ->
                        toHtml
                            { onChange = view.onChange
                            , onCheck = view.onCheck
                            , onFocus = view.onFocus
                            , onBlur = view.onBlur
                            , onAdd = view.onAdd
                            , onRemove = view.onRemove
                            , errorToString = view.errorToString
                            , fieldView = view.fieldView
                            , checkboxFieldView = view.checkboxFieldView
                            , groupView = view.groupView
                            , repeatableFieldsGroupView = view.repeatableFieldsGroupView
                            , repeatableFieldView = view.repeatableFieldView
                            , path = view.path ++ [ idx ]
                            , root = child
                            }
                    )
        , identifier = identifier
        , errors = visibleErrors view.root |> List.map (view.errorToString attrs)
        , class = String.join " " classList
        }


repeatableToHtml : View id msg -> Html msg
repeatableToHtml view =
    let
        ({ identifier } as attrs) =
            Tree.value view.root

        children =
            Tree.children view.root

        childrenCount =
            List.length children

        inputsView idx child =
            let
                childPath =
                    view.path ++ [ idx ]

                removeFieldsButtonOnClick =
                    view.onRemove identifier childPath

                removeFieldsButtonCopy =
                    attrs.removeFieldsButtonCopy

                removeFieldButtonEnabled =
                    childrenCount > attrs.repeatableMin
            in
            view.repeatableFieldView
                { field =
                    toHtml
                        { onChange = view.onChange
                        , onCheck = view.onCheck
                        , onFocus = view.onFocus
                        , onBlur = view.onBlur
                        , onAdd = view.onAdd
                        , onRemove = view.onRemove
                        , errorToString = view.errorToString
                        , fieldView = view.fieldView
                        , checkboxFieldView = view.checkboxFieldView
                        , groupView = view.groupView
                        , repeatableFieldsGroupView = view.repeatableFieldsGroupView
                        , repeatableFieldView = view.repeatableFieldView
                        , path = childPath
                        , root = child
                        }
                , removeFieldsButton =
                    \attrList ->
                        Html.button
                            (Attributes.class "remove-fields"
                                :: Attributes.disabled (not removeFieldButtonEnabled)
                                :: Events.preventDefaultOn "click"
                                    (Json.Decode.succeed
                                        ( removeFieldsButtonOnClick
                                        , True
                                        )
                                    )
                                :: attrList
                            )
                            [ Html.text removeFieldsButtonCopy
                            ]
                , index = idx
                , removeFieldsButtonCopy = removeFieldsButtonCopy
                , removeFieldsButtonOnClick =
                    if removeFieldButtonEnabled then
                        Just removeFieldsButtonOnClick

                    else
                        Nothing
                , class = inputId view.root childPath ++ "-repeat"
                , attributes = attrs
                }

        addFieldsButtonEnabled =
            case attrs.repeatableMax of
                Just max ->
                    childrenCount < max

                Nothing ->
                    True
    in
    view.repeatableFieldsGroupView
        { legendText = attrs.label
        , fields = List.indexedMap inputsView children
        , class = String.join " " attrs.classList
        , addFieldsButton =
            \attrList ->
                Html.button
                    (Attributes.class "add-fields"
                        :: Attributes.disabled (not addFieldsButtonEnabled)
                        :: Events.preventDefaultOn "click"
                            (Json.Decode.succeed
                                ( view.onAdd identifier view.path
                                , True
                                )
                            )
                        :: attrList
                    )
                    [ Html.text attrs.addFieldsButtonCopy ]
        , addFieldsButtonOnClick =
            if addFieldsButtonEnabled then
                Just (view.onAdd identifier view.path)

            else
                Nothing
        , errors = visibleErrors view.root |> List.map (view.errorToString attrs)
        , path = view.path
        , attributes = attrs
        }


inputToHtml :
    View id msg
    -> String
    -> List (Html.Attribute msg)
    -> (List (Html.Attribute msg) -> Html msg)
inputToHtml view inputType htmlAttrs element =
    let
        unwrappedField =
            Tree.value view.root

        inputHtml =
            Html.input
                (List.concat
                    [ htmlAttrs
                    , Attributes.type_ inputType
                        :: (Internal.Value.toString unwrappedField.value
                                |> Maybe.withDefault ""
                                |> Attributes.value
                           )
                        :: onInputWithSelection
                            (\inputStr ->
                                view.onChange unwrappedField.identifier
                                    view.path
                                    (inputStringToValue view.root inputStr)
                            )
                        :: textInputHtmlAttributes view
                    , element
                    ]
                )
                []
    in
    if isAutocompleteable view.root then
        Html.div
            []
            [ inputHtml
            , Html.datalist
                [ Attributes.id (datalistId view.root view.path)
                , Attributes.attribute "role" "listbox"
                ]
                (List.map
                    (\( opt, _ ) -> Html.option [ Attributes.value opt ] [])
                    unwrappedField.options
                )
            ]

    else
        inputHtml


textAreaToHtml : View id msg -> (List (Html.Attribute msg) -> Html msg)
textAreaToHtml view element =
    let
        { value, autogrow, identifier } =
            Tree.value view.root

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
                [ onInputWithSelection
                    (\inputStr ->
                        view.onChange identifier
                            view.path
                            (inputStringToValue view.root inputStr)
                    )
                    :: Attributes.value valueStr
                    :: textInputHtmlAttributes view
                , element
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
                                :: element
                            , autogrowAttrs
                            ]
                        )
                        [ Html.text (valueStr ++ "\n") ]
                    ]

                else
                    []
               )
        )


selectToHtml : View id msg -> (List (Html.Attribute msg) -> Html msg)
selectToHtml view element =
    let
        { identifier } =
            Tree.value view.root

        unwappedField =
            Tree.value view.root
    in
    Html.select
        (Attributes.id (inputId view.root view.path)
            :: Attributes.required unwappedField.isRequired
            :: Attributes.disabled unwappedField.disabled
            :: onInputWithSelection
                (\inputStr ->
                    view.onChange identifier
                        view.path
                        (inputStringToValue view.root inputStr)
                )
            :: Events.onFocus (view.onFocus identifier view.path)
            :: Events.onBlur (view.onBlur identifier view.path)
            :: nameAttribute view.root
            :: ariaLabeledByAttribute view.root view.path
            :: ariaDescribedByAttribute view.root view.path
            :: ariaInvalidAttribute view.root
            :: element
        )
        (Html.option [] []
            :: List.indexedMap
                (\index ( optionText, optionValue ) ->
                    Html.option
                        [ Attributes.selected (optionValue == unwappedField.value)
                        , Attributes.value (String.fromInt index)
                        ]
                        [ Html.text optionText ]
                )
                unwappedField.options
        )


radioToHtml : View id msg -> (List (Html.Attribute msg) -> Html msg)
radioToHtml view element =
    let
        { identifier } =
            Tree.value view.root

        unwrappedField =
            Tree.value view.root
    in
    Html.div
        [ Attributes.class "radios"
        , Attributes.attribute "role" "radiogroup"
        , ariaLabeledByAttribute view.root view.path
        , ariaDescribedByAttribute view.root view.path
        ]
        (List.indexedMap
            (\index ( optionText, optionValue ) ->
                Html.div
                    []
                    [ Html.input
                        (Attributes.id (radioOptionId view.root (view.path ++ [ index ]))
                            :: Attributes.checked (optionValue == unwrappedField.value)
                            :: Attributes.required unwrappedField.isRequired
                            :: Attributes.disabled unwrappedField.disabled
                            :: Attributes.value (String.fromInt index)
                            :: Attributes.type_ "radio"
                            :: onInputWithSelection
                                (\inputStr ->
                                    view.onChange identifier
                                        view.path
                                        (inputStringToValue view.root inputStr)
                                )
                            :: Events.onFocus (view.onFocus identifier view.path)
                            :: Events.onBlur (view.onBlur identifier view.path)
                            :: nameAttribute view.root
                            :: ariaInvalidAttribute view.root
                            :: element
                        )
                        []
                    , Html.label
                        [ Attributes.for (radioOptionId view.root (view.path ++ [ index ]))
                        , Attributes.class "label-inline"
                        ]
                        [ Html.text optionText ]
                    ]
            )
            unwrappedField.options
        )


checkboxToHtml : View id msg -> Html msg
checkboxToHtml view =
    let
        ({ identifier } as attrs) =
            Tree.value view.root

        inputHtml : List (Html.Attribute msg) -> Html msg
        inputHtml element =
            Html.input
                (List.concat
                    [ Attributes.type_ "checkbox"
                        :: (attrs.value
                                |> Internal.Value.toBool
                                |> Maybe.map Attributes.checked
                                |> Maybe.withDefault (Attributes.class "")
                           )
                        :: Events.onCheck (view.onCheck identifier view.path)
                        :: textInputHtmlAttributes view
                    , element
                    ]
                )
                []
    in
    view.checkboxFieldView
        { labelHtml = labelToHtml attrs.label view.path view.root
        , inputHtml = inputHtml
        , errors = visibleErrors view.root |> List.map (view.errorToString attrs)
        , hintHtml =
            \attrList ->
                case attrs.hint of
                    Just hintText ->
                        Html.div
                            (Attributes.class "hint"
                                :: Attributes.id (hintId view.root view.path)
                                :: attrList
                            )
                            [ Html.text hintText ]

                    Nothing ->
                        Html.text ""
        , path = view.path
        , class = String.join " " attrs.classList
        , attributes = attrs
        }


valueAttribute :
    (String -> Html.Attribute msg)
    -> Internal.Value.Value
    -> Html.Attribute msg
valueAttribute f inputValue =
    case inputValue of
        Internal.Value.Invalid ->
            f ""

        _ ->
            Internal.Value.toString inputValue
                |> Maybe.map f
                |> Maybe.withDefault (Attributes.class "")


textInputHtmlAttributes : View id msg -> List (Html.Attribute msg)
textInputHtmlAttributes view =
    let
        node =
            Tree.value view.root
    in
    List.concat
        [ if isAutocompleteable view.root then
            [ Attributes.autocomplete True
            , Attributes.list (datalistId view.root view.path)
            ]

          else
            [ Attributes.autocomplete False ]
        , [ Attributes.placeholder (Maybe.withDefault "" node.placeholder)
          , Attributes.id (inputId view.root view.path)
          , Attributes.required node.isRequired
          , Attributes.disabled node.disabled
          , Events.onFocus (view.onFocus node.identifier view.path)
          , Events.onBlur (view.onBlur node.identifier view.path)
          , nameAttribute view.root
          , valueAttribute Attributes.min node.min
          , valueAttribute Attributes.max node.max
          , ariaDescribedByAttribute view.root view.path
          , ariaInvalidAttribute view.root
          ]
        , if List.member node.inputType [ Text, TextArea ] then
            [ selectionStartAttribute node.selectionStart
            , selectionEndAttribute node.selectionEnd
            ]

          else
            []
        ]


inputId : Node id -> List Int -> String
inputId input path =
    String.join "-" (inputIdString input :: List.map String.fromInt path)


labelId : Node id -> List Int -> String
labelId input path =
    inputId input path ++ "-label"


hintId : Node id -> List Int -> String
hintId input path =
    inputId input path ++ "-hint"


radioOptionId : Node id -> List Int -> String
radioOptionId input path =
    inputId input path ++ "-option"


datalistId : Node id -> List Int -> String
datalistId input path =
    inputId input path ++ "-datalist"


visibleErrors : Node id -> List (Error id)
visibleErrors input =
    let
        params =
            Tree.value input
    in
    case ( params.status, params.inputType ) of
        ( Touched, _ ) ->
            params.errors

        ( _, Repeatable _ ) ->
            params.errors

        ( _, Group ) ->
            params.errors

        _ ->
            params.errors
                |> List.filter
                    (\err ->
                        case err of
                            IsBlank _ ->
                                False

                            PatternError _ ->
                                False

                            _ ->
                                True
                    )


groupView : GroupView id msg -> Html msg
groupView { fields, legendText, errors, class } =
    Html.fieldset
        [ Attributes.class class ]
        (List.concat
            [ (case legendText of
                Just str ->
                    Html.legend [] [ Html.text str ]

                Nothing ->
                    Html.text ""
              )
                :: fields
            , [ viewErrors errors ]
            ]
        )


repeatableFieldsGroupView : RepeatableFieldsGroupView id msg -> Html msg
repeatableFieldsGroupView { legendText, addFieldsButton, fields, errors, class } =
    Html.fieldset
        [ Attributes.class "group-repeatable"
        , Attributes.class class
        ]
        [ case legendText of
            Just str ->
                Html.legend [] [ Html.text str ]

            Nothing ->
                Html.text ""
        , Html.div [] fields
        , addFieldsButton []
        , viewErrors errors
        ]


repeatableFieldView : RepeatableFieldView id msg -> Html msg
repeatableFieldView { field, removeFieldsButton, class } =
    Html.div
        [ Attributes.class "group-repeat"
        , Attributes.class class
        ]
        [ field
        , removeFieldsButton []
        ]


fieldView : FieldView id msg -> Html msg
fieldView { attributes, labelHtml, inputHtml, errors, hintHtml, class } =
    Html.div
        [ Attributes.class "field"
        , Attributes.classList
            [ ( "required", attributes.isRequired )
            , ( "with-errors", not (List.isEmpty errors) )
            ]
        , Attributes.class class
        ]
        [ labelHtml []
        , Html.div
            [ Attributes.class "input-wrapper" ]
            [ inputHtml [] ]
        , case errors of
            [] ->
                hintHtml []

            _ ->
                viewErrors errors
        ]


checkboxFieldView : FieldView id msg -> Html msg
checkboxFieldView { attributes, labelHtml, inputHtml, errors, hintHtml, class } =
    Html.div
        [ Attributes.class "field"
        , Attributes.classList
            [ ( "required", attributes.isRequired )
            , ( "with-errors", not (List.isEmpty errors) )
            ]
        , Attributes.class class
        ]
        [ Html.div
            [ Attributes.class "input-wrapper" ]
            [ inputHtml []
            , labelHtml []
            ]
        , case errors of
            [] ->
                hintHtml []

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


nameAttribute : Node id -> Html.Attribute msg
nameAttribute input =
    (Tree.value input).name
        |> Maybe.map Attributes.name
        |> Maybe.withDefault (Attributes.class "")


ariaDescribedByAttribute : Node id -> List Int -> Html.Attribute msg
ariaDescribedByAttribute input path =
    (Tree.value input).hint
        |> Maybe.map
            (\_ -> Attributes.attribute "aria-describedby" (hintId input path))
        |> Maybe.withDefault (Attributes.class "")


ariaLabeledByAttribute : Node id -> List Int -> Html.Attribute msg
ariaLabeledByAttribute input path =
    (Tree.value input).label
        |> Maybe.map
            (\_ -> Attributes.attribute "aria-labelledby" (labelId input path))
        |> Maybe.withDefault (Attributes.class "")


ariaInvalidAttribute : Node id -> Html.Attribute msg
ariaInvalidAttribute input =
    if List.isEmpty (visibleErrors input) then
        Attributes.class ""

    else
        Attributes.attribute "aria-invalid" "true"


selectionStartAttribute : Int -> Html.Attribute msg
selectionStartAttribute position =
    Attributes.property "selectionStart" (Json.Encode.int position)


selectionEndAttribute : Int -> Html.Attribute msg
selectionEndAttribute position =
    Attributes.property "selectionEnd" (Json.Encode.int position)


onInputWithSelection : (String -> { selectionStart : Int, selectionEnd : Int } -> msg) -> Html.Attribute msg
onInputWithSelection tagger =
    Events.on "input"
        (Json.Decode.map2 tagger
            (Json.Decode.at [ "target", "value" ] Json.Decode.string)
            (Json.Decode.map2 (\start end -> { selectionStart = start, selectionEnd = end })
                (Json.Decode.oneOf
                    [ Json.Decode.at [ "target", "selectionStart" ] Json.Decode.int
                    , Json.Decode.succeed 0
                    ]
                )
                (Json.Decode.oneOf
                    [ Json.Decode.at [ "target", "selectionEnd" ] Json.Decode.int
                    , Json.Decode.succeed 0
                    ]
                )
            )
        )


inputIdString : Node id -> String
inputIdString input =
    let
        { name, inputType } =
            Tree.value input
    in
    name
        |> Maybe.withDefault
            (inputTypeToString inputType)


inputTypeToString : FieldType id err -> String
inputTypeToString type_ =
    case type_ of
        Text ->
            "text"

        StrictAutocomplete ->
            "text"

        TextArea ->
            "textarea"

        Email ->
            "email"

        Password ->
            "password"

        Integer ->
            "integer"

        Float ->
            "float"

        Month ->
            "month"

        Date ->
            "date"

        LocalDatetime ->
            "datetime-local"

        Select ->
            "select"

        Radio ->
            "radio"

        Checkbox ->
            "checkbox"

        Repeatable _ ->
            "repeatable"

        Group ->
            "group"


isAutocompleteable : Node id -> Bool
isAutocompleteable input =
    let
        { inputType, options } =
            Tree.value input
    in
    case inputType of
        Text ->
            not (List.isEmpty options)

        StrictAutocomplete ->
            True

        _ ->
            False
