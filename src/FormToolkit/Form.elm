module FormToolkit.Form exposing
    ( Form(..), init
    , Msg, update
    , Attribute, toHtml, onChange, onSubmit, elementHtml, toInputGroup
    , Error, errors
    , validate, hasBlankValues, hasErrors, isValid, clear
    , encodeValues, setValues
    )

{-| Separate in typed and untyped?


# Init

@docs Form, init


# Update

@docs Msg, update


# View

@docs Attribute, toHtml, onChange, onSubmit, elementHtml, toInputGroup


# Validation

@docs Error, errors
@docs validate, hasBlankValues, hasErrors, isValid, clear


# JSON

@docs encodeValues, setValues

-}

import Dict exposing (Dict)
import Dict.Extra as Dict
import FormToolkit.Error as Error
import FormToolkit.Input as InputAttribute exposing (Input)
import Html
    exposing
        ( Html
        , button
        , div
        , fieldset
        , p
        )
import Html.Attributes as A
    exposing
        ( attribute
        , autocomplete
        , checked
        , class
        , classList
        , for
        , id
        , novalidate
        , required
        , selected
        , step
        , type_
        , value
        )
import Html.Events as Html
    exposing
        ( onBlur
        , onFocus
        , onInput
        , preventDefaultOn
        )
import Internal.Input
import Internal.Markdown as Markdown
import Internal.Tree as Tree
import Internal.Value exposing (Value)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Result


{-| Form
-}
type Form id
    = Form (Input id)


type alias Attributes id msg =
    { onSubmit : Maybe msg
    , onChange : Maybe (Msg -> msg)
    , elements : List ( id, Html msg )
    }



-- INIT


{-| TODO
-}
init : List (Input id) -> Form id
init inputs =
    Form (Tree.branch Internal.Input.root inputs)


initAttributes : Attributes id msg
initAttributes =
    { onSubmit = Nothing
    , onChange = Nothing
    , elements = []
    }


{-| TODO
-}
type Msg
    = InputChanged (List Int) String
    | InputChecked (List Int) Bool
    | InputFocused (List Int)
    | InputBlured (List Int)
    | InputsAdded (List Int)
    | InputsRemoved (List Int)


{-| TODO
-}
update : Msg -> Form id -> Form id
update msg (Form root) =
    case msg of
        InputChanged path str ->
            Form (Tree.update path (updateInput str) root)

        InputChecked path bool ->
            Form (Tree.update path (updateInputWithBool bool) root)

        InputFocused path ->
            Form (Tree.update path resetInputStatus root)

        InputBlured path ->
            Form (Tree.update path validateInput root)

        InputsAdded path ->
            case Tree.getValue path root |> Maybe.map .inputType of
                Just (Internal.Input.Repeatable template) ->
                    Form (Tree.update path (Tree.push template) root)

                _ ->
                    Form root

        InputsRemoved path ->
            Form (Tree.remove path root)


updateInput : String -> Input id -> Input id
updateInput string =
    Tree.updateValue (Internal.Input.updateWithString string)


updateInputWithBool : Bool -> Input id -> Input id
updateInputWithBool bool =
    Tree.updateValue
        (Internal.Input.update
            (Internal.Value.fromBoolean bool)
        )


resetInputStatus : Input id -> Input id
resetInputStatus =
    Tree.updateValue Internal.Input.resetStatus


validateInput : Input id -> Input id
validateInput =
    Tree.updateValue Internal.Input.validate


{-| TODO
-}
type Attribute id msg
    = Attribute (Attributes id msg -> Attributes id msg)


{-| TODO
-}
onSubmit : msg -> Attribute id msg
onSubmit tagger =
    Attribute (\attrs -> { attrs | onSubmit = Just tagger })


{-| TODO
-}
onChange : (Msg -> msg) -> Attribute id msg
onChange tagger =
    Attribute (\attrs -> { attrs | onChange = Just tagger })


{-| TODO
-}
elementHtml : id -> Html msg -> Attribute id msg
elementHtml id html =
    Attribute
        (\attrs ->
            { attrs | elements = ( id, html ) :: attrs.elements }
        )


{-| TODO
-}
setValues : Dict String Decode.Value -> Form id -> Form id
setValues values (Form root) =
    Form (Tree.map (Tree.updateValue (setValuesHelp values)) root)


setValuesHelp : Dict String Decode.Value -> Internal.Input.Input id -> Internal.Input.Input id
setValuesHelp values input =
    Dict.get input.name values
        |> Maybe.map
            (\val ->
                case Decode.decodeValue valueDecoder val of
                    Ok inputValue ->
                        Internal.Input.update inputValue input

                    Err _ ->
                        input
            )
        |> Maybe.withDefault input


{-| TODO
-}
clear : Form id -> Form id
clear (Form root) =
    Form
        (Tree.map
            (Tree.updateValue
                (Internal.Input.update Internal.Value.blank)
            )
            root
        )


valueDecoder : Decode.Decoder Value
valueDecoder =
    Decode.oneOf
        [ Decode.map Internal.Value.fromBoolean Decode.bool
        , Decode.map Internal.Value.fromString Decode.string
        , Decode.map Internal.Value.fromInt Decode.int
        , Decode.map Internal.Value.fromFloat Decode.float
        ]


{-| TODO
-}
toInputGroup : List (InputAttribute.Attribute id) -> Form id -> Input id
toInputGroup attributes (Form root) =
    InputAttribute.group attributes (Tree.children root)



-- VALUES


{-| TODO
-}
encodeValues : Form id -> Encode.Value
encodeValues (Form root) =
    Encode.object (encodeHelp root [])


encodeHelp :
    Input id
    -> List ( String, Encode.Value )
    -> List ( String, Encode.Value )
encodeHelp inputElement acc =
    let
        input =
            Tree.value inputElement
    in
    case input.inputType of
        Internal.Input.Group ->
            List.foldl encodeHelp acc (Tree.children inputElement)

        Internal.Input.Repeatable _ ->
            ( input.name
            , Encode.list
                (\e -> Encode.object (encodeHelp e []))
                (Tree.children inputElement)
            )
                :: acc

        _ ->
            ( input.name, Internal.Value.encode input.value ) :: acc



-- UPDATE


{-| Error
-}
type Error id
    = InputError (Maybe id) Error.Error


{-| TODO
-}
errors : Form id -> List (Error id)
errors (Form tree) =
    Tree.foldr
        (\v errs ->
            let
                input =
                    Tree.value v
            in
            case Internal.Input.error input of
                Just err ->
                    InputError input.identifier err :: errs

                Nothing ->
                    errs
        )
        []
        tree


{-| TODO
-}
validate : Form id -> Form id
validate (Form root) =
    Form (Tree.mapValues Internal.Input.validate root)


check : Form id -> Result (Error id) ()
check (Form root) =
    Tree.foldl
        (\node ->
            let
                input =
                    Tree.value node
            in
            Result.andThen
                (\_ ->
                    Internal.Input.check input
                        |> Result.map (always ())
                        |> Result.mapError (InputError input.identifier)
                )
        )
        (Ok ())
        root


{-| TODO
-}
isValid : Form id -> Bool
isValid form =
    check form
        |> Result.map (always True)
        |> Result.withDefault False


{-| TODO
-}
hasBlankValues : Form id -> Bool
hasBlankValues (Form tree) =
    Tree.any (Tree.value >> Internal.Input.isBlank) tree


{-| TODO
-}
hasErrors : Form id -> Bool
hasErrors (Form tree) =
    Tree.any (\v -> Internal.Input.error (Tree.value v) /= Nothing) tree



-- VIEW


{-| TODO
-}
toHtml : List (Attribute id msg) -> Form id -> Html msg
toHtml attrList (Form root) =
    let
        attrs =
            List.foldl (\(Attribute f) a -> f a) initAttributes attrList
    in
    Html.form
        [ class "indexing-form simple-form"
        , attrs.onSubmit
            |> Maybe.map Html.onSubmit
            |> Maybe.withDefault (class "")
        , novalidate True
        ]
        [ fieldset [] [ elementToHtml attrs [] root ]
        , submitButtonHtml []
        ]


elementToHtml : Attributes id msg -> List Int -> Input id -> Html msg
elementToHtml attrs path node =
    let
        input =
            Tree.value node
    in
    case input.inputType of
        Internal.Input.Group ->
            let
                children =
                    Tree.children node
                        |> List.indexedMap
                            (\idx -> elementToHtml attrs (path ++ [ idx ]))

                inputLabel =
                    case input.label of
                        Just label ->
                            Html.legend [] [ Html.text label ]

                        Nothing ->
                            Html.text ""
            in
            fieldset
                [ id (identifier input.name path)
                , classList
                    [ ( "inline", input.inline )
                    , ( "stacked", not input.inline )
                    ]
                ]
                (inputLabel :: children)

        Internal.Input.Repeatable _ ->
            let
                children =
                    Tree.children node

                inputs =
                    children
                        |> List.indexedMap
                            (\idx ->
                                templateHtml attrs
                                    (path ++ [ idx ])
                                    (List.length children /= (idx + 1))
                            )
            in
            fieldset
                [ id (identifier input.name path) ]
                [ div [] (legend input.label :: inputs)
                , addInputsButton attrs path
                ]

        Internal.Input.Text ->
            inputToHtml attrs "text" path input []
                |> wrapInput path input

        Internal.Input.Email ->
            inputToHtml attrs "email" path input []
                |> wrapInput path input

        Internal.Input.Password ->
            inputToHtml attrs "password" path input []
                |> wrapInput path input

        Internal.Input.TextArea ->
            textAreaToHtml attrs path input
                |> wrapInput path input

        Internal.Input.Integer ->
            inputToHtml attrs "number" path input [ step "1" ]
                |> wrapInput path input

        Internal.Input.Float ->
            inputToHtml attrs "number" path input [ step "1" ]
                |> wrapInput path input

        Internal.Input.Date ->
            inputToHtml attrs "date" path input []
                |> wrapInput path input

        Internal.Input.Month ->
            inputToHtml attrs "month" path input []
                |> wrapInput path input

        Internal.Input.Select ->
            selectToHtml attrs path input
                |> wrapInput path input

        Internal.Input.Radio ->
            radioToHtml attrs path input
                |> wrapInput path input

        Internal.Input.Checkbox ->
            checkboxToHtml attrs path input
                |> wrapInput path input

        Internal.Input.Element elementId ->
            attrs.elements
                |> List.filter (Tuple.first >> (==) elementId)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault (Html.text "")


inputToHtml :
    Attributes id msg
    -> String
    -> List Int
    -> Internal.Input.Input id
    -> List (Html.Attribute msg)
    -> Html msg
inputToHtml attrs inputType path input htmlAttrs =
    Html.input
        (htmlAttrs
            ++ type_ inputType
            :: valueAttribute value input.value
            :: onInputChanged attrs path
            :: inputAttrs attrs path input
        )
        []


textAreaToHtml : Attributes id msg -> List Int -> Internal.Input.Input id -> Html msg
textAreaToHtml attrs path input =
    let
        value =
            Internal.Value.toString input.value
                |> Result.withDefault ""
    in
    div
        [ class "grow-wrap"
        , attribute "data-replicated-value" value
        ]
        [ Html.textarea
            (onInputChanged attrs path
                :: A.value value
                :: inputAttrs attrs path input
            )
            []
        ]


checkboxToHtml : Attributes id msg -> List Int -> Internal.Input.Input id -> Html msg
checkboxToHtml attrs path input =
    Html.input
        (type_ "checkbox"
            :: (Internal.Value.toBoolean input.value
                    |> Result.map checked
                    |> Result.withDefault (class "")
               )
            :: (case attrs.onChange of
                    Just tagger ->
                        Html.onCheck (InputChecked path >> tagger)

                    Nothing ->
                        class ""
               )
            :: inputAttrs attrs path input
        )
        []


selectToHtml : Attributes id msg -> List Int -> Internal.Input.Input id -> Html msg
selectToHtml attrs path { name, isRequired, options, value } =
    Html.select
        [ id (identifier name path)
        , required isRequired
        , onInputChanged attrs path
        , onInputFocused attrs path
        , onInputBlured attrs path
        ]
        (Html.option [] []
            :: List.indexedMap
                (\index ( optionText, optionValue ) ->
                    Html.option
                        [ selected (optionValue == value)
                        , A.value (String.fromInt index)
                        ]
                        [ Html.text optionText ]
                )
                options
        )


radioToHtml : Attributes id msg -> List Int -> Internal.Input.Input id -> Html msg
radioToHtml attrs path { name, isRequired, options, value } =
    Html.div
        [ class "radios" ]
        (List.indexedMap
            (\index ( optionText, optionValue ) ->
                let
                    optionId =
                        identifier (name ++ String.fromInt index) path
                in
                Html.div []
                    [ Html.input
                        [ id optionId
                        , checked (optionValue == value)
                        , required isRequired
                        , A.value (String.fromInt index)
                        , onInputChanged attrs path
                        , onInputFocused attrs path
                        , onInputBlured attrs path
                        , type_ "radio"
                        ]
                        []
                    , Html.label
                        [ for optionId ]
                        [ Html.text optionText ]
                    ]
            )
            options
        )


addInputsButton : Attributes id msg -> List Int -> Html msg
addInputsButton attrs path =
    button
        [ class "add-fields"
        , case attrs.onChange of
            Just tagger ->
                preventDefaultOn "click"
                    (Decode.succeed
                        ( tagger (InputsAdded path), True )
                    )

            Nothing ->
                class ""
        ]
        [ Html.text "add"
        ]


templateHtml : Attributes id msg -> List Int -> Bool -> Input id -> Html msg
templateHtml attributes path isLast inputElement =
    div
        [ class "group-repeat" ]
        [ elementToHtml attributes path inputElement
        , if isLast then
            button
                [ class "remove-fields"
                , case attributes.onChange of
                    Just tagger ->
                        preventDefaultOn "click"
                            (Decode.succeed
                                ( tagger (InputsRemoved path), True )
                            )

                    Nothing ->
                        class ""
                ]
                [ Html.text "Remove"
                ]

          else
            Html.text ""
        ]


wrapInput : List Int -> Internal.Input.Input id -> Html msg -> Html msg
wrapInput path { hint, name, status, isRequired, label } inputHtml =
    div
        [ class "field"
        , classList [ ( "required", isRequired ) ]
        ]
        [ Html.label
            [ for (identifier name path) ]
            [ Html.text (Maybe.withDefault name label) ]
        , div
            [ class "input-wrapper" ]
            [ inputHtml ]
        , case Internal.Input.errorMessage status of
            Just msg ->
                p [ class "error" ] [ Html.text msg ]

            Nothing ->
                case hint of
                    Just msg ->
                        div
                            [ class "hint" ]
                            [ Markdown.toHtml msg ]

                    Nothing ->
                        Html.text ""
        ]


identifier : String -> List Int -> String
identifier name path =
    String.join "-" (name :: List.map String.fromInt path)


legend : Maybe String -> Html msg
legend label =
    label
        |> Maybe.map (\t -> Html.legend [] [ Html.text t ])
        |> Maybe.withDefault (Html.text "")


valueAttribute : (String -> Html.Attribute msg) -> Value -> Html.Attribute msg
valueAttribute f value =
    Internal.Value.toString value
        |> Result.map f
        |> Result.withDefault (class "")


inputAttrs : Attributes id msg -> List Int -> Internal.Input.Input id -> List (Html.Attribute msg)
inputAttrs attrs path { name, isRequired, placeholder, min, max } =
    [ id (identifier name path)
    , required isRequired
    , autocomplete False
    , onInputFocused attrs path
    , onInputBlured attrs path
    , A.placeholder (Maybe.withDefault "" placeholder)
    , valueAttribute A.min min
    , valueAttribute A.max max
    ]


submitButtonHtml : List (Html.Attribute msg) -> Html msg
submitButtonHtml attrs =
    button
        (id "form-submit-button" :: attrs)
        [ Html.text "Submit" ]


onInputChanged : Attributes id msg -> List Int -> Html.Attribute msg
onInputChanged attrs path =
    case attrs.onChange of
        Just tagger ->
            onInput (InputChanged path >> tagger)

        Nothing ->
            class ""


onInputFocused : Attributes id msg -> List Int -> Html.Attribute msg
onInputFocused attrs path =
    case attrs.onChange of
        Just tagger ->
            onFocus (tagger (InputFocused path))

        Nothing ->
            class ""


onInputBlured : Attributes id msg -> List Int -> Html.Attribute msg
onInputBlured attrs path =
    case attrs.onChange of
        Just tagger ->
            onBlur (tagger (InputBlured path))

        Nothing ->
            class ""
