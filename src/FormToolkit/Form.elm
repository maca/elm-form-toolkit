module FormToolkit.Form exposing
    ( Form(..), Msg
    , init, update
    , toHtml, onChange, onSubmit
    , get, getMaybe, getInput
    , validate, isValid, hasBlankValues, hasErrors, clear
    , encodeValues, setValues
    , Error, errors
    )

{-|

@docs Form, Msg

@docs init, update


# View

@docs toHtml, onChange, onSubmit


# Traversing

@docs get, getMaybe, getInput


# Validation

@docs validate, isValid, hasBlankValues, hasErrors, isValid, clear


# JSON

@docs encodeValues, setValues

-}

import Dict exposing (Dict)
import Dict.Extra as Dict
import FormToolkit.Error exposing (Error)
import FormToolkit.Input as Input
import FormToolkit.Value as Value exposing (Value)
import Html
    exposing
        ( Html
        , button
        , div
        , fieldset
        , p
        , text
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
import Internal.Input as Input exposing (Input)
import Internal.Markdown as Markdown
import Internal.Tree as Tree exposing (Tree)
import Internal.Value exposing (Value)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Result


{-|

@docs Form, Msg

@docs init, update

Attributes

@docs onChange, onSubmit

Json Values

@docs setValues, updateValues

-}
type Form id
    = Form (Tree (Input id))


type Error id
    = InputError (Maybe id) Input.Error
    | InputNotFound id


type alias Attributes msg =
    { onSubmit : Maybe msg
    , onChange : Maybe (Msg -> msg)
    }


type Attribute msg
    = Attribute (Attributes msg -> Attributes msg)


type Msg
    = InputChanged (List Int) String
    | InputChecked (List Int) Bool
    | InputFocused (List Int)
    | InputBlured (List Int)
    | InputsAdded (List Int)
    | InputsRemoved (List Int)



-- INIT


init : List (Tree (Input id)) -> Form id
init inputs =
    Form (Tree.branch Input.root inputs)


initAttributes : Attributes msg
initAttributes =
    { onSubmit = Nothing
    , onChange = Nothing
    }



-- Interface


onSubmit : msg -> Attribute msg
onSubmit tagger =
    Attribute (\attrs -> { attrs | onSubmit = Just tagger })


onChange : (Msg -> msg) -> Attribute msg
onChange tagger =
    Attribute (\attrs -> { attrs | onChange = Just tagger })


setValues : Dict String Decode.Value -> Form id -> Form id
setValues values (Form root) =
    Form (Tree.map (Tree.updateValue (setValuesHelp values)) root)


setValuesHelp : Dict String Decode.Value -> Input a -> Input a
setValuesHelp values input =
    Dict.get input.name values
        |> Maybe.map
            (\val ->
                case Decode.decodeValue valueDecoder val of
                    Ok inputValue ->
                        Input.update inputValue input

                    Err _ ->
                        input
            )
        |> Maybe.withDefault input


clear : Form id -> Form id
clear (Form root) =
    Form (Tree.map (Tree.updateValue (Input.update Value.blank)) root)


valueDecoder : Decode.Decoder Value
valueDecoder =
    Decode.oneOf
        [ Decode.map Value.boolean Decode.bool
        , Decode.map Value.string Decode.string
        , Decode.map Value.int Decode.int
        , Decode.map Value.float Decode.float
        ]



-- VALUES


encodeValues : Form id -> Encode.Value
encodeValues (Form root) =
    Encode.object (encodeHelp root [])


encodeHelp :
    Tree (Input a)
    -> List ( String, Encode.Value )
    -> List ( String, Encode.Value )
encodeHelp element acc =
    let
        input =
            Tree.value element
    in
    case input.inputType of
        Input.Group ->
            List.foldl encodeHelp acc (Tree.children element)

        Input.Repeatable _ ->
            ( input.name
            , Encode.list
                (\e -> Encode.object (encodeHelp e []))
                (Tree.children element)
            )
                :: acc

        _ ->
            ( input.name, Internal.Value.encode input.value ) :: acc



-- UPDATE


validate : Form id -> Form id
validate (Form root) =
    Form (Tree.mapValues Input.validate root)


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
                    Input.check input
                        |> Result.map (always ())
                        |> Result.mapError (InputError input.identifier)
                )
        )
        (Ok ())
        root


isValid : Form id -> Bool
isValid form =
    check form
        |> Result.map (always True)
        |> Result.withDefault False


hasBlankValues : Form id -> Bool
hasBlankValues (Form tree) =
    Tree.any (Tree.value >> Input.isBlank) tree


hasErrors : Form id -> Bool
hasErrors (Form tree) =
    Tree.any (\v -> Input.error (Tree.value v) /= Nothing) tree


errors : Form id -> List (Error id)
errors (Form tree) =
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
        tree


getInput : id -> Form id -> Result (Error id) (Input id)
getInput id (Form root) =
    root
        |> Tree.find (Tree.value >> .identifier >> (==) (Just id))
        |> Maybe.map (Ok << Tree.value)
        |> Maybe.withDefault (Err (InputNotFound id))


get : id -> (Value -> Result Input.Error a) -> Form id -> Result (Error id) a
get id f form =
    getInput id form
        |> Result.andThen
            (\input ->
                Input.check input
                    |> Result.andThen f
                    |> Result.mapError (InputError input.identifier)
            )


getMaybe :
    id
    -> (Value -> Result Input.Error a)
    -> Form id
    -> Result (Error id) (Maybe a)
getMaybe id f =
    get id (Value.toMaybe f)



-- UPDATE FUNC


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
                Just (Input.Repeatable template) ->
                    Form (Tree.update path (Tree.push template) root)

                _ ->
                    Form root

        InputsRemoved path ->
            Form (Tree.remove path root)


updateInput : String -> Tree (Input a) -> Tree (Input a)
updateInput string =
    Tree.updateValue (Input.updateWithString string)


updateInputWithBool : Bool -> Tree (Input a) -> Tree (Input a)
updateInputWithBool bool =
    Tree.updateValue (Input.update (Value.boolean bool))


resetInputStatus : Tree (Input a) -> Tree (Input a)
resetInputStatus =
    Tree.updateValue Input.resetStatus


validateInput : Tree (Input a) -> Tree (Input a)
validateInput =
    Tree.updateValue Input.validate



-- VIEW


toHtml : List (Attribute msg) -> Form id -> Html msg
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
        [ fieldset
            []
            [ elementToHtml attrs [] root ]
        , submitButtonHtml []
        ]


elementToHtml : Attributes msg -> List Int -> Tree (Input a) -> Html msg
elementToHtml attrs path node =
    let
        input =
            Tree.value node
    in
    case input.inputType of
        Input.Group ->
            let
                children =
                    Tree.children node
                        |> List.indexedMap
                            (\idx -> elementToHtml attrs (path ++ [ idx ]))
            in
            fieldset
                [ id (identifier input.name path)
                , classList
                    [ ( "inline", input.inline )
                    , ( "stacked", not input.inline )
                    ]
                ]
                ((case input.label of
                    Just label ->
                        Html.legend [] [ text label ]

                    Nothing ->
                        text ""
                 )
                    :: children
                )

        Input.Repeatable _ ->
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

        Input.Text ->
            inputToHtml attrs "text" path input []
                |> wrapInput path input

        Input.Email ->
            inputToHtml attrs "email" path input []
                |> wrapInput path input

        Input.Password ->
            inputToHtml attrs "password" path input []
                |> wrapInput path input

        Input.TextArea ->
            textAreaToHtml attrs path input
                |> wrapInput path input

        Input.Integer ->
            inputToHtml attrs "number" path input [ step "1" ]
                |> wrapInput path input

        Input.Float ->
            inputToHtml attrs "number" path input [ step "1" ]
                |> wrapInput path input

        Input.Date ->
            inputToHtml attrs "date" path input []
                |> wrapInput path input

        Input.Month ->
            inputToHtml attrs "month" path input []
                |> wrapInput path input

        Input.Select ->
            selectToHtml attrs path input
                |> wrapInput path input

        Input.Radio ->
            radioToHtml attrs path input
                |> wrapInput path input

        Input.Checkbox ->
            checkboxToHtml attrs path input
                |> wrapInput path input


submitButtonHtml : List (Html.Attribute msg) -> Html msg
submitButtonHtml attrs =
    button
        (id "form-submit-button" :: attrs)
        [ text "Submit" ]


inputToHtml : Attributes msg -> String -> List Int -> Input a -> List (Html.Attribute msg) -> Html msg
inputToHtml attrs inputType path input htmlAttrs =
    Html.input
        (htmlAttrs
            ++ type_ inputType
            :: valueAttribute value input.value
            :: onInputChanged attrs path
            :: inputAttrs attrs path input
        )
        []


textAreaToHtml : Attributes msg -> List Int -> Input a -> Html msg
textAreaToHtml attrs path input =
    let
        value =
            Internal.Value.toString input.value
                |> Maybe.withDefault ""
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


checkboxToHtml : Attributes msg -> List Int -> Input a -> Html msg
checkboxToHtml attrs path input =
    Html.input
        (type_ "checkbox"
            :: (Value.toBool input.value
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


inputAttrs : Attributes msg -> List Int -> Input a -> List (Html.Attribute msg)
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


selectToHtml : Attributes msg -> List Int -> Input a -> Html msg
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
                        [ text optionText ]
                )
                options
        )


valueAttribute : (String -> Html.Attribute msg) -> Value -> Html.Attribute msg
valueAttribute f value =
    Internal.Value.toString value
        |> Maybe.map f
        |> Maybe.withDefault (class "")


radioToHtml : Attributes msg -> List Int -> Input a -> Html msg
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
                    , Html.label [ for optionId ] [ text optionText ]
                    ]
            )
            options
        )


addInputsButton : Attributes msg -> List Int -> Html msg
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
        [ text "add"
        ]


templateHtml : Attributes msg -> List Int -> Bool -> Tree (Input a) -> Html msg
templateHtml attributes path isLast element =
    div
        [ class "group-repeat" ]
        [ elementToHtml attributes path element
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
                [ text "Remove"
                ]

          else
            text ""
        ]


wrapInput : List Int -> Input a -> Html msg -> Html msg
wrapInput path { hint, name, status, isRequired, label } inputHtml =
    div
        [ class "field"
        , classList [ ( "required", isRequired ) ]
        ]
        [ Html.label
            [ for (identifier name path) ]
            [ text (Maybe.withDefault name label) ]
        , div
            [ class "input-wrapper" ]
            [ inputHtml ]
        , case Input.errorMessage status of
            Just msg ->
                p [ class "error" ] [ text msg ]

            Nothing ->
                case hint of
                    Just msg ->
                        div
                            [ class "hint" ]
                            [ Markdown.toHtml msg ]

                    Nothing ->
                        text ""
        ]


identifier : String -> List Int -> String
identifier name path =
    String.join "-" (name :: List.map String.fromInt path)


legend : Maybe String -> Html msg
legend label =
    label
        |> Maybe.map (\t -> Html.legend [] [ text t ])
        |> Maybe.withDefault (text "")


onInputChanged : Attributes msg -> List Int -> Html.Attribute msg
onInputChanged attrs path =
    case attrs.onChange of
        Just tagger ->
            onInput (InputChanged path >> tagger)

        Nothing ->
            class ""


onInputFocused : Attributes msg -> List Int -> Html.Attribute msg
onInputFocused attrs path =
    case attrs.onChange of
        Just tagger ->
            onFocus (tagger (InputFocused path))

        Nothing ->
            class ""


onInputBlured : Attributes msg -> List Int -> Html.Attribute msg
onInputBlured attrs path =
    case attrs.onChange of
        Just tagger ->
            onBlur (tagger (InputBlured path))

        Nothing ->
            class ""
