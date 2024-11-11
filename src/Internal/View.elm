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
import Internal.Field as Field exposing (Field, Status(..))
import Internal.Value
import Json.Decode
import RoseTree.Tree as Tree


type alias Field id val =
    Field.Field id val (Error id val)


type alias View id val msg =
    { attributes : ViewAttributes id val msg
    , path : List Int
    , field : Field id val
    }


type alias ViewAttributes id val msg =
    { onChange : List Int -> Internal.Value.Value val -> msg
    , onFocus : List Int -> msg
    , onBlur : List Int -> msg
    , onAdd : List Int -> msg
    , onRemove : List Int -> msg
    , errorToString : Error id val -> String
    , fieldView : FieldView msg -> Html msg
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
    }


type alias GroupView id msg =
    { legendText : Maybe String
    , fields : List (Html msg)
    , identifier : Maybe id
    , errors : List String
    }


type alias RepeatableFieldsGroupView msg =
    { legendText : Maybe String
    , fields : List (Html msg)
    , addFieldsButton : UserAttributes -> Html msg
    , addFieldsButtonOnClick : Maybe msg
    , errors : List String
    , path : List Int
    }


type alias RepeatableFieldView msg =
    { field : Html msg
    , removeFieldsButton : UserAttributes -> Html msg
    , index : Int
    , removeFieldsButtonCopy : String
    , removeFieldsButtonOnClick : Maybe msg
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
    , field : Field id val
    }
    -> View id val msg
init { events, path, field } =
    { attributes =
        { onChange = events.onChange
        , onFocus = events.onFocus
        , onBlur = events.onBlur
        , onAdd = events.onAdd
        , onRemove = events.onRemove
        , errorToString = errorToString
        , fieldView = inputView
        , groupView = groupView
        , repeatableFieldsGroupView = repeatableFieldsGroupView
        , repeatableFieldView = repeatableFieldView
        }
    , path = path
    , field =
        Decode.validateAndDecode (Decode.succeed ()) field
            |> Tuple.first
    }


partial : id -> View id val msg -> Maybe (View id val msg)
partial id { field, attributes } =
    findNode id field
        |> Maybe.map (\( found, path ) -> View attributes path found)


findNode : id -> Field id val -> Maybe ( Field id val, List Int )
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
                }
    in
    case unwrappedField.inputType of
        Field.Group ->
            groupToHtml attributes path field

        Field.Repeatable _ ->
            repeatableToHtml attributes path field

        Field.Text ->
            wrapInput (inputToHtml attributes "text" path field [])

        Field.StrictAutocomplete ->
            wrapInput (inputToHtml attributes "text" path field [])

        Field.Email ->
            wrapInput (inputToHtml attributes "email" path field [])

        Field.Password ->
            wrapInput (inputToHtml attributes "password" path field [])

        Field.TextArea ->
            wrapInput (textAreaToHtml attributes path field)

        Field.Integer ->
            inputToHtml attributes "number" path field [ Attributes.step "1" ]
                |> wrapInput

        Field.Float ->
            inputToHtml attributes "number" path field [ Attributes.step "1" ]
                |> wrapInput

        Field.Date ->
            wrapInput (inputToHtml attributes "date" path field [])

        Field.Month ->
            wrapInput (inputToHtml attributes "month" path field [])

        Field.Select ->
            wrapInput (selectToHtml attributes path field)

        Field.Radio ->
            wrapInput (radioToHtml attributes path field)

        Field.Checkbox ->
            wrapInput (checkboxToHtml attributes path field)


labelToHtml : Maybe String -> List Int -> Field id val -> (UserAttributes -> Html msg)
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


groupToHtml : ViewAttributes id val msg -> List Int -> Field id val -> Html msg
groupToHtml attributes path input =
    let
        { identifier, label } =
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
        }


repeatableToHtml : ViewAttributes id val msg -> List Int -> Field id val -> Html msg
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
                    Tree.value child |> .removeFieldsButtonCopy

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
    ViewAttributes id val msg
    -> String
    -> List Int
    -> Field id val
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
                        :: valueAttribute Attributes.value unwrappedField.value
                        :: Events.onInput
                            (attributes.onChange path << Field.strToValue input)
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
    ViewAttributes id val msg
    -> List Int
    -> Field id val
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
                [ Events.onInput (attributes.onChange path << Field.strToValue input)
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
    -> Field id val
    -> (UserAttributes -> Html msg)
selectToHtml { onChange, onFocus, onBlur } path input element =
    let
        unwappedField =
            Tree.value input
    in
    Html.select
        (Attributes.id (inputId input path)
            :: Attributes.required unwappedField.isRequired
            :: Events.onInput (onChange path << Field.strToValue input)
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
    ViewAttributes id val msg
    -> List Int
    -> Field id val
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
                            :: Attributes.value (String.fromInt index)
                            :: Attributes.type_ "radio"
                            :: Events.onInput (onChange path << Field.strToValue input)
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
            unwrappedField.options
        )


checkboxToHtml :
    ViewAttributes id val msg
    -> List Int
    -> Field id val
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


textInputHtmlAttributes : ViewAttributes id val msg -> List Int -> Field id val -> List (Html.Attribute msg)
textInputHtmlAttributes attributes path input =
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
          , Events.onFocus (attributes.onFocus path)
          , Events.onBlur (attributes.onBlur path)
          , nameAttribute input
          , valueAttribute Attributes.min (Field.min input)
          , valueAttribute Attributes.max (Field.max input)
          , ariaDescribedByAttribute input path
          , ariaInvalidAttribute input
          ]
        ]


inputId : Field id val -> List Int -> String
inputId input path =
    String.join "-" (Field.inputIdString input :: List.map String.fromInt path)


labelId : Field id val -> List Int -> String
labelId input path =
    inputId input path ++ "-label"


hintId : Field id val -> List Int -> String
hintId input path =
    inputId input path ++ "-hint"


radioOptionId : Field id val -> List Int -> String
radioOptionId input path =
    inputId input path ++ "-option"


datalistId : Field id val -> List Int -> String
datalistId input path =
    inputId input path ++ "-datalist"


visibleErrors : Field id val -> List (Error id val)
visibleErrors input =
    let
        { errors, status, inputType } =
            Tree.value input
    in
    case ( status, inputType ) of
        ( Touched, _ ) ->
            errors

        ( _, Field.Repeatable _ ) ->
            errors

        ( _, Field.Group ) ->
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
groupView { fields, legendText, errors } =
    Html.fieldset []
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
repeatableFieldsGroupView { legendText, addFieldsButton, fields, errors } =
    Html.fieldset []
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
repeatableFieldView { field, removeFieldsButton } =
    Html.div
        [ Attributes.class "group-repeat" ]
        [ field
        , removeFieldsButton defaultAttributes
        ]


inputView : FieldView msg -> Html msg
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


nameAttribute : Field id val -> Html.Attribute msg
nameAttribute input =
    Field.name input
        |> Maybe.map Attributes.name
        |> Maybe.withDefault (Attributes.class "")


ariaDescribedByAttribute : Field id val -> List Int -> Html.Attribute msg
ariaDescribedByAttribute input path =
    Field.hint input
        |> Maybe.map
            (\_ -> Attributes.attribute "aria-describedby" (hintId input path))
        |> Maybe.withDefault (Attributes.class "")


ariaLabeledByAttribute : Field id val -> List Int -> Html.Attribute msg
ariaLabeledByAttribute input path =
    Field.label input
        |> Maybe.map
            (\_ -> Attributes.attribute "aria-labelledby" (labelId input path))
        |> Maybe.withDefault (Attributes.class "")


ariaInvalidAttribute : Field id val -> Html.Attribute msg
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
