module FormToolkit.Form exposing
    ( Form(..), init
    , Msg, update
    , Attribute, toHtml, onChange, onSubmit
    , elementHtml, toInputGroup
    , Copy(..), viewCopy, defaultCopies
    , Error(..), errors
    , validate, hasBlankValues, hasErrors, isValid, clear
    , encodeValues, setValues
    )

{-| Separate in typed and untyped?


# Init

@docs Form, init


# Update

@docs Msg, update


# View

@docs Attribute, toHtml, onChange, onSubmit
@docs elementHtml, toInputGroup
@docs Copy, viewCopy, defaultCopies


# Validation

@docs Error, errors
@docs validate, hasBlankValues, hasErrors, isValid, clear


# JSON

@docs encodeValues, setValues

-}

import Dict exposing (Dict)
import Dict.Extra as Dict
import FormToolkit.Input as Input exposing (Input)
import FormToolkit.Value as Value
import Html
    exposing
        ( Html
        , button
        , div
        , fieldset
        , p
        )
import Html.Attributes as Attributes
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
import Html.Events as Events
    exposing
        ( onBlur
        , onFocus
        , onInput
        , preventDefaultOn
        )
import Internal.Input
import Internal.Markdown as Markdown
import Internal.Tree as Tree exposing (Tree)
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
    , onChange : Maybe (Msg id -> msg)
    , viewCopy : Copy id -> Html msg
    , elements : List ( id, Html msg )
    }



-- INIT


{-| TODO
-}
init : List (Input id) -> Form id
init inputs =
    Form (Input.group [] inputs)


initAttributes : Attributes id msg
initAttributes =
    { onSubmit = Nothing
    , onChange = Nothing
    , viewCopy = defaultCopies
    , elements = []
    }


{-| TODO
-}
type Msg id
    = InputChanged (List Int) String
    | InputChecked (List Int) Bool
    | InputFocused (List Int)
    | InputBlured (List Int)
    | InputsAdded (List Int)
    | InputsRemoved (List Int)


type alias InputTree id =
    Tree.Tree (Internal.Input.Input id Input.Error)


{-| TODO
-}
update : Msg id -> Form id -> Form id
update msg ((Form group) as form) =
    case msg of
        InputChanged path str ->
            updateHelp path (updateInput str) form

        InputChecked path bool ->
            updateHelp path (updateInputWithBool bool) form

        InputFocused path ->
            updateHelp path resetInputStatus form

        InputBlured path ->
            updateHelp path validateInput form

        InputsAdded path ->
            case
                Tree.getValue path (Input.toTree group)
                    |> Maybe.map .inputType
            of
                Just (Internal.Input.Repeatable template) ->
                    updateHelp path (Tree.push template) form

                _ ->
                    Form group

        InputsRemoved path ->
            Form (Input.fromTree (Tree.remove path (Input.toTree group)))


updateHelp : List Int -> (InputTree id -> InputTree id) -> Form id -> Form id
updateHelp path func (Form group) =
    Form (Input.fromTree (Tree.update path func (Input.toTree group)))


updateInput : String -> InputTree id -> InputTree id
updateInput string =
    Tree.updateValue (Internal.Input.updateWithString string)


updateInputWithBool : Bool -> InputTree id -> InputTree id
updateInputWithBool bool =
    Tree.updateValue
        (Internal.Input.update
            (Internal.Value.fromBool bool)
        )


resetInputStatus : InputTree id -> InputTree id
resetInputStatus =
    Tree.updateValue Internal.Input.resetStatus


validateInput : InputTree id -> InputTree id
validateInput =
    Tree.updateValue
        (\input ->
            case Input.check input of
                Ok _ ->
                    { input | status = Internal.Input.Valid }

                Err err ->
                    { input | status = Internal.Input.WithError err }
        )


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
onChange : (Msg id -> msg) -> Attribute id msg
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
setValues values (Form group) =
    Form (mapInput (Tree.updateValue (setValuesHelp values)) group)


setValuesHelp :
    Dict String Decode.Value
    -> Internal.Input.Input id Input.Error
    -> Internal.Input.Input id Input.Error
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
clear (Form group) =
    Form
        (mapInput
            (Tree.updateValue
                (Internal.Input.update Internal.Value.blank)
            )
            group
        )


valueDecoder : Decode.Decoder Value
valueDecoder =
    Decode.oneOf
        [ Decode.map Internal.Value.fromBool Decode.bool
        , Decode.map Internal.Value.fromString Decode.string
        , Decode.map Internal.Value.fromInt Decode.int
        , Decode.map Internal.Value.fromFloat Decode.float
        ]


{-| TODO
-}
toInputGroup : List (Input.Attribute id) -> Form id -> Input id
toInputGroup attributes (Form group) =
    Tree.children (Input.toTree group)
        |> List.map Input.fromTree
        |> Input.group attributes


{-| TODO
-}
type Copy id
    = Submit
    | AddInputs
    | RemoveInputs
    | ErrorMessage (Maybe id) Input.Error


{-| TODO
-}
viewCopy : (Copy id -> Html msg) -> Attribute id msg
viewCopy func =
    Attribute (\attrs -> { attrs | viewCopy = func })


{-| TODO
-}
defaultCopies : Copy id -> Html msg
defaultCopies copy =
    case copy of
        Submit ->
            Html.text "Submit"

        AddInputs ->
            Html.text "Add"

        RemoveInputs ->
            Html.text "Remove"

        ErrorMessage _ err ->
            Html.text (Input.errorToEnglish err)


{-| TODO
-}
encodeValues : Form id -> Encode.Value
encodeValues (Form group) =
    Encode.object (encodeHelp group [])


encodeHelp :
    Input id
    -> List ( String, Encode.Value )
    -> List ( String, Encode.Value )
encodeHelp inputElement acc =
    let
        tree =
            Input.toTree inputElement

        input =
            Tree.value tree

        children =
            Tree.children tree
    in
    case input.inputType of
        Internal.Input.Group ->
            List.foldl (encodeHelp << Input.fromTree) acc children

        Internal.Input.Repeatable _ ->
            ( input.name
            , Encode.list
                (\e -> Encode.object (encodeHelp (Input.fromTree e) []))
                children
            )
                :: acc

        _ ->
            ( input.name, Internal.Value.encode input.value ) :: acc


{-| Error
-}
type Error id
    = InputError (Maybe id) Input.Error


{-| TODO
-}
errors : Form id -> List (Error id)
errors (Form group) =
    Tree.foldr
        (\v errs ->
            let
                input =
                    Tree.value v
            in
            case Input.error input of
                Just err ->
                    InputError input.identifier err :: errs

                Nothing ->
                    errs
        )
        []
        (Input.toTree group)


{-| TODO
-}
validate : Form id -> Form id
validate (Form group) =
    Form
        (Input.fromTree
            (Tree.mapValues
                (\input ->
                    -- Duplicated
                    case Input.check input of
                        Ok _ ->
                            { input | status = Internal.Input.Valid }

                        Err err ->
                            { input | status = Internal.Input.WithError err }
                )
                (Input.toTree group)
            )
        )


check : Form id -> Result (Error id) ()
check (Form group) =
    Tree.foldl
        (\node ->
            let
                input =
                    Tree.value node
            in
            Result.andThen
                (\_ ->
                    Input.check input
                        |> Result.map (always ())
                        |> Result.mapError (InputError input.identifier)
                )
        )
        (Ok ())
        (Input.toTree group)


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
hasBlankValues (Form group) =
    Tree.any (Tree.value >> Internal.Input.isBlank) (Input.toTree group)


{-| TODO
-}
hasErrors : Form id -> Bool
hasErrors (Form group) =
    Tree.any (\v -> Input.error (Tree.value v) /= Nothing) (Input.toTree group)



-- VIEW


{-| TODO
-}
toHtml : List (Attribute id msg) -> Form id -> Html msg
toHtml attrList (Form group) =
    let
        attrs =
            List.foldl (\(Attribute f) a -> f a) initAttributes attrList
    in
    Html.form
        [ class "indexing-form simple-form"
        , attrs.onSubmit
            |> Maybe.map Events.onSubmit
            |> Maybe.withDefault (class "")
        , novalidate True
        ]
        [ fieldset [] [ treeToHtml attrs [] group ]
        , submitButtonHtml attrs
        ]


treeToHtml : Attributes id msg -> List Int -> Input id -> Html msg
treeToHtml attrs path node =
    let
        tree =
            Input.toTree node

        input =
            Tree.value tree
    in
    case input.inputType of
        Internal.Input.Group ->
            let
                children =
                    Tree.children tree
                        |> List.indexedMap
                            (\idx ->
                                Input.fromTree
                                    >> treeToHtml attrs (path ++ [ idx ])
                            )

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
                    Tree.children tree

                inputs =
                    children
                        |> List.indexedMap
                            (\idx ->
                                Input.fromTree
                                    >> templateHtml attrs
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
                |> wrapInput attrs path input

        Internal.Input.Email ->
            inputToHtml attrs "email" path input []
                |> wrapInput attrs path input

        Internal.Input.Password ->
            inputToHtml attrs "password" path input []
                |> wrapInput attrs path input

        Internal.Input.TextArea ->
            textAreaToHtml attrs path input
                |> wrapInput attrs path input

        Internal.Input.Integer ->
            inputToHtml attrs "number" path input [ step "1" ]
                |> wrapInput attrs path input

        Internal.Input.Float ->
            inputToHtml attrs "number" path input [ step "1" ]
                |> wrapInput attrs path input

        Internal.Input.Date ->
            inputToHtml attrs "date" path input []
                |> wrapInput attrs path input

        Internal.Input.Month ->
            inputToHtml attrs "month" path input []
                |> wrapInput attrs path input

        Internal.Input.Select ->
            selectToHtml attrs path input
                |> wrapInput attrs path input

        Internal.Input.Radio ->
            radioToHtml attrs path input
                |> wrapInput attrs path input

        Internal.Input.Checkbox ->
            checkboxToHtml attrs path input
                |> wrapInput attrs path input

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
    -> Internal.Input.Input id Input.Error
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


textAreaToHtml :
    Attributes id msg
    -> List Int
    -> Internal.Input.Input id Input.Error
    -> Html msg
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
                :: Attributes.value value
                :: inputAttrs attrs path input
            )
            []
        ]


checkboxToHtml :
    Attributes id msg
    -> List Int
    -> Internal.Input.Input id Input.Error
    -> Html msg
checkboxToHtml attrs path input =
    Html.input
        (type_ "checkbox"
            :: (Internal.Value.toBool input.value
                    |> Result.map checked
                    |> Result.withDefault (class "")
               )
            :: (case attrs.onChange of
                    Just tagger ->
                        Events.onCheck (InputChecked path >> tagger)

                    Nothing ->
                        class ""
               )
            :: inputAttrs attrs path input
        )
        []


selectToHtml :
    Attributes id msg
    -> List Int
    -> Internal.Input.Input id Input.Error
    -> Html msg
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
                        , Attributes.value (String.fromInt index)
                        ]
                        [ Html.text optionText ]
                )
                options
        )


radioToHtml : Attributes id msg -> List Int -> Internal.Input.Input id Input.Error -> Html msg
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
                        , Attributes.value (String.fromInt index)
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
        [ attrs.viewCopy AddInputs
        ]


templateHtml :
    Attributes id msg
    -> List Int
    -> Bool
    -> Input id
    -> Html msg
templateHtml attributes path isLast inputElement =
    div
        [ class "group-repeat" ]
        [ treeToHtml attributes path inputElement
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
                [ attributes.viewCopy RemoveInputs ]

          else
            Html.text ""
        ]


wrapInput :
    Attributes id msg
    -> List Int
    -> Internal.Input.Input id Input.Error
    -> Html msg
    -> Html msg
wrapInput attrs path ({ hint, name, isRequired, label } as input) inputHtml =
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
        , case errorMessage input of
            Just message ->
                p [ class "error" ] [ attrs.viewCopy message ]

            Nothing ->
                case hint of
                    Just msg ->
                        div
                            [ class "hint" ]
                            [ Markdown.toHtml msg ]

                    Nothing ->
                        Html.text ""
        ]


{-| TODO
-}
errorMessage : Internal.Input.Input id Input.Error -> Maybe (Copy id)
errorMessage input =
    Maybe.map (ErrorMessage input.identifier) (Input.error input)


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


inputAttrs :
    Attributes id msg
    -> List Int
    -> Internal.Input.Input id Input.Error
    -> List (Html.Attribute msg)
inputAttrs attrs path { name, isRequired, placeholder, min, max } =
    [ id (identifier name path)
    , required isRequired
    , autocomplete False
    , onInputFocused attrs path
    , onInputBlured attrs path
    , Attributes.placeholder (Maybe.withDefault "" placeholder)
    , valueAttribute Attributes.min min
    , valueAttribute Attributes.max max
    ]


submitButtonHtml : Attributes id msg -> Html msg
submitButtonHtml attrs =
    button [ id "form-submit-button" ] [ attrs.viewCopy Submit ]


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


mapInput :
    (Tree (Internal.Input.Input id Input.Error)
     -> Tree (Internal.Input.Input id Input.Error)
    )
    -> Input id
    -> Input id
mapInput func input =
    Input.fromTree (Tree.map func (Input.toTree input))
