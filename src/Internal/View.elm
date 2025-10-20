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

import FormToolkit.Error exposing (Error(..))
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.Field as Field exposing (Field, Status(..))
import Internal.Value
import Json.Decode
import Json.Encode
import RoseTree.Tree as Tree


type alias Field id =
    Field.Field id (Error id)


type alias View id msg =
    { attributes : ViewAttributes id msg
    , path : List Int
    , field : Field id
    }


type alias ViewAttributes id msg =
    { onChange : List Int -> Internal.Value.Value -> { selectionStart : Int, selectionEnd : Int } -> msg
    , onFocus : List Int -> msg
    , onBlur : List Int -> msg
    , onAdd : List Int -> msg
    , onRemove : List Int -> msg
    , errorToString : Error id -> String
    , fieldView : FieldView msg -> Html msg
    , checkboxFieldView : FieldView msg -> Html msg
    , groupView : GroupView id msg -> Html msg
    , repeatableFieldsGroupView : RepeatableFieldsGroupView msg -> Html msg
    , repeatableFieldView : RepeatableFieldView msg -> Html msg
    }


type alias FieldView msg =
    { isRequired : Bool
    , label : UserAttributes -> Html msg
    , input : UserAttributes -> Html msg
    , hint : UserAttributes -> Html msg
    , errors : List String
    , path : List Int
    , class : String
    }


type alias GroupView id msg =
    { legendText : Maybe String
    , fields : List (Html msg)
    , identifier : Maybe id
    , errors : List String
    , class : String
    }


type alias RepeatableFieldsGroupView msg =
    { legendText : Maybe String
    , fields : List (Html msg)
    , addFieldsButton : UserAttributes -> Html msg
    , addFieldsButtonOnClick : Maybe msg
    , errors : List String
    , path : List Int
    , class : String
    }


type alias RepeatableFieldView msg =
    { field : Html msg
    , removeFieldsButton : UserAttributes -> Html msg
    , index : Int
    , removeFieldsButtonCopy : String
    , removeFieldsButtonOnClick : Maybe msg
    , class : String
    }


init :
    { events :
        { onChange : List Int -> Internal.Value.Value -> { selectionStart : Int, selectionEnd : Int } -> msg
        , onFocus : List Int -> msg
        , onBlur : List Int -> msg
        , onAdd : List Int -> msg
        , onRemove : List Int -> msg
        }
    , path : List Int
    , field : Field id
    }
    -> View id msg
init { events, path, field } =
    { attributes =
        { onChange = events.onChange
        , onFocus = events.onFocus
        , onBlur = events.onBlur
        , onAdd = events.onAdd
        , onRemove = events.onRemove
        , errorToString = FormToolkit.Error.toEnglish
        , fieldView = fieldView
        , checkboxFieldView = checkboxFieldView
        , groupView = groupView
        , repeatableFieldsGroupView = repeatableFieldsGroupView
        , repeatableFieldView = repeatableFieldView
        }
    , path = path
    , field = field
    }


partial : id -> View id msg -> Maybe (View id msg)
partial id { field, attributes } =
    findNode id field
        |> Maybe.map (\( found, path ) -> View attributes path found)


findNode : id -> Field id -> Maybe ( Field id, List Int )
findNode id =
    Tree.foldWithPath
        (\path node foundPath ->
            if .identifier (Tree.value node) == Just id then
                Just ( node, path )

            else
                foundPath
        )
        Nothing


toHtml : View id msg -> Html msg
toHtml { field, path, attributes } =
    let
        unwrappedField =
            Tree.value field

        wrapInput : (UserAttributes -> Html msg) -> Html msg
        wrapInput inputHtml =
            attributes.fieldView
                { isRequired = unwrappedField.isRequired
                , label = labelToHtml unwrappedField.label path field
                , input = inputHtml
                , errors = visibleErrors field |> List.map attributes.errorToString
                , hint =
                    \attrList ->
                        case unwrappedField.hint of
                            Just hintText ->
                                Html.div
                                    (Attributes.class "hint"
                                        :: Attributes.id (hintId field path)
                                        :: userProvidedAttributes attrList
                                    )
                                    [ Html.text hintText ]

                            Nothing ->
                                Html.text ""
                , path = path
                , class = String.join " " unwrappedField.classList
                }
    in
    case ( unwrappedField.visible, unwrappedField.inputType ) of
        ( False, _ ) ->
            Html.text ""

        ( True, Field.Group ) ->
            groupToHtml attributes path field

        ( True, Field.Repeatable _ ) ->
            repeatableToHtml attributes path field

        ( True, Field.Text ) ->
            wrapInput (inputToHtml attributes "text" path field [])

        ( True, Field.StrictAutocomplete ) ->
            wrapInput (inputToHtml attributes "text" path field [])

        ( True, Field.Email ) ->
            wrapInput (inputToHtml attributes "email" path field [])

        ( True, Field.Password ) ->
            wrapInput (inputToHtml attributes "password" path field [])

        ( True, Field.TextArea ) ->
            wrapInput (textAreaToHtml attributes path field)

        ( True, Field.Integer ) ->
            inputToHtml attributes "number" path field [ valueAttribute Attributes.step (Field.step field) ]
                |> wrapInput

        ( True, Field.Float ) ->
            inputToHtml attributes "number" path field [ valueAttribute Attributes.step (Field.step field) ]
                |> wrapInput

        ( True, Field.Date ) ->
            wrapInput (inputToHtml attributes "date" path field [])

        ( True, Field.Month ) ->
            wrapInput (inputToHtml attributes "month" path field [])

        ( True, Field.LocalDatetime ) ->
            wrapInput (inputToHtml attributes "datetime-local" path field [])

        ( True, Field.Select ) ->
            wrapInput (selectToHtml attributes path field)

        ( True, Field.Radio ) ->
            wrapInput (radioToHtml attributes path field)

        ( True, Field.Checkbox ) ->
            checkboxToHtml attributes path field

        ( True, Field.Error errorList ) ->
            viewErrors (List.map attributes.errorToString errorList)


labelToHtml : Maybe String -> List Int -> Field id -> (UserAttributes -> Html msg)
labelToHtml label path input element =
    case label of
        Just str ->
            Html.label
                (Attributes.for (inputId input path)
                    :: Attributes.id (labelId input path)
                    :: (if (Tree.value input).inputType == Field.Checkbox then
                            Attributes.class "label-inline" :: userProvidedAttributes element

                        else
                            userProvidedAttributes element
                       )
                )
                [ Html.text str ]

        Nothing ->
            Html.text ""


groupToHtml : ViewAttributes id msg -> List Int -> Field id -> Html msg
groupToHtml attributes path input =
    let
        { identifier, label, classList } =
            Tree.value input
    in
    attributes.groupView
        { legendText = label
        , fields =
            Tree.children input
                |> List.indexedMap
                    (\idx -> View attributes (path ++ [ idx ]) >> toHtml)
        , identifier = identifier
        , errors = visibleErrors input |> List.map attributes.errorToString
        , class = String.join " " classList
        }


repeatableToHtml : ViewAttributes id msg -> List Int -> Field id -> Html msg
repeatableToHtml attributes path input =
    let
        unwrappedField =
            Tree.value input

        children =
            Tree.children input

        childrenCount =
            List.length children

        inputsView idx child =
            let
                childPath =
                    path ++ [ idx ]

                removeFieldsButtonOnClick =
                    attributes.onRemove childPath

                removeFieldsButtonCopy =
                    unwrappedField.removeFieldsButtonCopy

                removeFieldButtonEnabled =
                    childrenCount > unwrappedField.repeatableMin
            in
            attributes.repeatableFieldView
                { field = toHtml (View attributes childPath child)
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
                                :: userProvidedAttributes attrList
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
                , class = inputId input childPath ++ "-repeat"
                }

        addFieldsButtonEnabled =
            case unwrappedField.repeatableMax of
                Just max ->
                    childrenCount < max

                Nothing ->
                    True
    in
    repeatableFieldsGroupView
        { legendText = unwrappedField.label
        , fields = List.indexedMap inputsView children
        , class = String.join " " unwrappedField.classList
        , addFieldsButton =
            \attrList ->
                Html.button
                    (Attributes.class "add-fields"
                        :: Attributes.disabled (not addFieldsButtonEnabled)
                        :: Events.preventDefaultOn "click"
                            (Json.Decode.succeed
                                ( attributes.onAdd path
                                , True
                                )
                            )
                        :: userProvidedAttributes attrList
                    )
                    [ Html.text unwrappedField.addFieldsButtonCopy ]
        , addFieldsButtonOnClick =
            if addFieldsButtonEnabled then
                Just (attributes.onAdd path)

            else
                Nothing
        , errors = visibleErrors input |> List.map attributes.errorToString
        , path = path
        }


inputToHtml :
    ViewAttributes id msg
    -> String
    -> List Int
    -> Field id
    -> List (Html.Attribute msg)
    -> (UserAttributes -> Html msg)
inputToHtml attributes inputType path input htmlAttrs element =
    let
        unwrappedField =
            Tree.value input

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
                                attributes.onChange path
                                    (Field.inputStringToValue input inputStr)
                            )
                        :: textInputHtmlAttributes attributes path input
                    , userProvidedAttributes element
                    ]
                )
                []
    in
    if Field.isAutocompleteable input then
        Html.div
            []
            [ inputHtml
            , Html.datalist
                [ Attributes.id (datalistId input path)
                , Attributes.attribute "role" "listbox"
                ]
                (List.map
                    (\( opt, _ ) -> Html.option [ Attributes.value opt ] [])
                    unwrappedField.options
                )
            ]

    else
        inputHtml


textAreaToHtml :
    ViewAttributes id msg
    -> List Int
    -> Field id
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
                [ onInputWithSelection
                    (\inputStr ->
                        attributes.onChange path
                            (Field.inputStringToValue input inputStr)
                    )
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
    ViewAttributes id msg
    -> List Int
    -> Field id
    -> (UserAttributes -> Html msg)
selectToHtml { onChange, onFocus, onBlur } path input element =
    let
        unwappedField =
            Tree.value input
    in
    Html.select
        (Attributes.id (inputId input path)
            :: Attributes.required unwappedField.isRequired
            :: Attributes.disabled unwappedField.disabled
            :: onInputWithSelection
                (\inputStr ->
                    onChange path
                        (Field.inputStringToValue input inputStr)
                )
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
                        [ Attributes.selected (optionValue == unwappedField.value)
                        , Attributes.value (String.fromInt index)
                        ]
                        [ Html.text optionText ]
                )
                unwappedField.options
        )


radioToHtml :
    ViewAttributes id msg
    -> List Int
    -> Field id
    -> (UserAttributes -> Html msg)
radioToHtml { onChange, onFocus, onBlur } path input element =
    let
        unwrappedField =
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
                            :: Attributes.checked (optionValue == unwrappedField.value)
                            :: Attributes.required unwrappedField.isRequired
                            :: Attributes.disabled unwrappedField.disabled
                            :: Attributes.value (String.fromInt index)
                            :: Attributes.type_ "radio"
                            :: onInputWithSelection
                                (\inputStr ->
                                    onChange path
                                        (Field.inputStringToValue input inputStr)
                                )
                            :: Events.onFocus (onFocus path)
                            :: Events.onBlur (onBlur path)
                            :: nameAttribute input
                            :: ariaInvalidAttribute input
                            :: userProvidedAttributes element
                        )
                        []
                    , Html.label
                        [ Attributes.for (radioOptionId input (path ++ [ index ]))
                        , Attributes.class "label-inline"
                        ]
                        [ Html.text optionText ]
                    ]
            )
            unwrappedField.options
        )


checkboxToHtml :
    ViewAttributes id msg
    -> List Int
    -> Field id
    -> Html msg
checkboxToHtml attributes path field =
    let
        unwrappedField =
            Tree.value field

        inputHtml : UserAttributes -> Html msg
        inputHtml element =
            Html.input
                (List.concat
                    [ Attributes.type_ "checkbox"
                        :: (unwrappedField.value
                                |> Internal.Value.toBool
                                |> Maybe.map Attributes.checked
                                |> Maybe.withDefault (Attributes.class "")
                           )
                        :: Events.onCheck
                            (\checked ->
                                attributes.onChange path
                                    (Internal.Value.fromBool checked)
                                    { selectionStart = 0, selectionEnd = 0 }
                            )
                        :: textInputHtmlAttributes attributes path field
                    , userProvidedAttributes element
                    ]
                )
                []
    in
    attributes.checkboxFieldView
        { isRequired = unwrappedField.isRequired
        , label = labelToHtml unwrappedField.label path field
        , input = inputHtml
        , errors = visibleErrors field |> List.map attributes.errorToString
        , hint =
            \attrList ->
                case unwrappedField.hint of
                    Just hintText ->
                        Html.div
                            (Attributes.class "hint"
                                :: Attributes.id (hintId field path)
                                :: userProvidedAttributes attrList
                            )
                            [ Html.text hintText ]

                    Nothing ->
                        Html.text ""
        , path = path
        , class = String.join " " unwrappedField.classList
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


textInputHtmlAttributes : ViewAttributes id msg -> List Int -> Field id -> List (Html.Attribute msg)
textInputHtmlAttributes attributes path input =
    let
        node =
            Tree.value input
    in
    List.concat
        [ if Field.isAutocompleteable input then
            [ Attributes.autocomplete True
            , Attributes.list (datalistId input path)
            ]

          else
            [ Attributes.autocomplete False ]
        , [ Attributes.placeholder (Maybe.withDefault "" (Field.placeholder input))
          , Attributes.id (inputId input path)
          , Attributes.required (Field.isRequired input)
          , Attributes.disabled node.disabled
          , Events.onFocus (attributes.onFocus path)
          , Events.onBlur (attributes.onBlur path)
          , nameAttribute input
          , valueAttribute Attributes.min (Field.min input)
          , valueAttribute Attributes.max (Field.max input)
          , ariaDescribedByAttribute input path
          , ariaInvalidAttribute input
          ]
        , if List.member node.inputType [ Field.Text, Field.TextArea ] then
            [ selectionStartAttribute node.selectionStart
            , selectionEndAttribute node.selectionEnd
            ]

          else
            []
        ]


inputId : Field id -> List Int -> String
inputId input path =
    String.join "-" (Field.inputIdString input :: List.map String.fromInt path)


labelId : Field id -> List Int -> String
labelId input path =
    inputId input path ++ "-label"


hintId : Field id -> List Int -> String
hintId input path =
    inputId input path ++ "-hint"


radioOptionId : Field id -> List Int -> String
radioOptionId input path =
    inputId input path ++ "-option"


datalistId : Field id -> List Int -> String
datalistId input path =
    inputId input path ++ "-datalist"


visibleErrors : Field id -> List (Error id)
visibleErrors input =
    let
        params =
            Tree.value input
    in
    case ( params.status, params.inputType ) of
        ( Touched, _ ) ->
            params.errors

        ( _, Field.Repeatable _ ) ->
            params.errors

        ( _, Field.Group ) ->
            params.errors

        _ ->
            params.errors
                |> List.filter
                    (\err ->
                        case err of
                            IsBlank _ ->
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


repeatableFieldsGroupView : RepeatableFieldsGroupView msg -> Html msg
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
        , addFieldsButton defaultAttributes
        , viewErrors errors
        ]


repeatableFieldView : RepeatableFieldView msg -> Html msg
repeatableFieldView { field, removeFieldsButton, class } =
    Html.div
        [ Attributes.class "group-repeat"
        , Attributes.class class
        ]
        [ field
        , removeFieldsButton defaultAttributes
        ]


fieldView : FieldView msg -> Html msg
fieldView { isRequired, label, input, errors, hint, class } =
    Html.div
        [ Attributes.class "field"
        , Attributes.classList
            [ ( "required", isRequired )
            , ( "with-errors", not (List.isEmpty errors) )
            ]
        , Attributes.class class
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


checkboxFieldView : FieldView msg -> Html msg
checkboxFieldView { isRequired, label, input, errors, hint, class } =
    Html.div
        [ Attributes.class "field"
        , Attributes.classList
            [ ( "required", isRequired )
            , ( "with-errors", not (List.isEmpty errors) )
            ]
        , Attributes.class class
        ]
        [ Html.div
            [ Attributes.class "input-wrapper" ]
            [ input defaultAttributes
            , label defaultAttributes
            ]
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


nameAttribute : Field id -> Html.Attribute msg
nameAttribute input =
    Field.name input
        |> Maybe.map Attributes.name
        |> Maybe.withDefault (Attributes.class "")


ariaDescribedByAttribute : Field id -> List Int -> Html.Attribute msg
ariaDescribedByAttribute input path =
    Field.hint input
        |> Maybe.map
            (\_ -> Attributes.attribute "aria-describedby" (hintId input path))
        |> Maybe.withDefault (Attributes.class "")


ariaLabeledByAttribute : Field id -> List Int -> Html.Attribute msg
ariaLabeledByAttribute input path =
    Field.label input
        |> Maybe.map
            (\_ -> Attributes.attribute "aria-labelledby" (labelId input path))
        |> Maybe.withDefault (Attributes.class "")


ariaInvalidAttribute : Field id -> Html.Attribute msg
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
