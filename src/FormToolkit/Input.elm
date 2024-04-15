module FormToolkit.Input exposing
    ( Input
    , text, textarea, email, password
    , integer, float
    , date, month
    , select, radio, checkbox
    , group, repeatable
    , elementPlaceholder
    , mapIdentifier
    , Attribute
    , name, identifier, value, required, label, hint, placeholder
    , options, min, max
    , inline, noattr
    , HtmlAttribute, toHtml, onChange
    , addInputsButtonContent, removeInputsButtonContent, errorToHtml
    , elementHtml
    , Msg, update
    , Error(..), error, check, errorToEnglish
    , clear
    , getValue
    , toJSON
    , fromTree, toTree
    )

{-|


# Inputs

@docs Input
@docs text, textarea, email, password
@docs integer, float
@docs date, month
@docs select, radio, checkbox
@docs group, repeatable
@docs elementPlaceholder
@docs mapIdentifier


# Attributes

@docs Attribute
@docs name, identifier, value, required, label, hint, placeholder
@docs options, min, max
@docs inline, noattr


# View

@docs HtmlAttribute, toHtml, onChange
@docs addInputsButtonContent, removeInputsButtonContent, errorToHtml
@docs elementHtml


# Update

@docs Msg, update


# Validation

@docs Error, error, check, errorToEnglish
@docs clear
@docs getValue


# JSON

@docs toJSON


# Etc

@docs fromTree, toTree

-}

import Dict exposing (Dict)
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.Input as Internal
import Internal.Markdown as Markdown
import Internal.Value
import Json.Decode as Decode
import Json.Encode as Encode
import RoseTree.Tree as Tree exposing (Tree)


{-| TODO
-}
type Input id
    = Input (Tree (Internal.Input id Error))


{-| TODO
-}
text : List (Attribute id) -> Input id
text =
    init Internal.Text


{-| TODO
-}
textarea : List (Attribute id) -> Input id
textarea =
    init Internal.TextArea


{-| TODO
-}
email : List (Attribute id) -> Input id
email =
    init Internal.Email


{-| TODO
-}
password : List (Attribute id) -> Input id
password =
    init Internal.Password


{-| TODO
-}
integer : List (Attribute id) -> Input id
integer =
    init Internal.Integer


{-| TODO
-}
float : List (Attribute id) -> Input id
float =
    init Internal.Float


{-| TODO
-}
date : List (Attribute id) -> Input id
date =
    init Internal.Date


{-| TODO
-}
month : List (Attribute id) -> Input id
month =
    init Internal.Month


{-| TODO
-}
select : List (Attribute id) -> Input id
select =
    init Internal.Select


{-| TODO
-}
radio : List (Attribute id) -> Input id
radio =
    init Internal.Radio


{-| TODO
-}
checkbox : List (Attribute id) -> Input id
checkbox =
    init Internal.Checkbox


{-| TODO
-}
group : List (Attribute id) -> List (Input id) -> Input id
group attributes inputs =
    List.map toTree inputs
        |> Tree.branch
            (Internal.init Internal.Group (unwrapAttrs attributes))
        |> Input


{-| TODO
-}
repeatable : List (Attribute id) -> Input id -> List (Input id) -> Input id
repeatable attributes template inputs =
    Input <|
        Tree.branch
            (Internal.init (Internal.Repeatable (toTree template))
                (unwrapAttrs attributes)
            )
            (if List.isEmpty inputs then
                [ toTree template ]

             else
                List.map toTree inputs
            )


{-| TODO
-}
elementPlaceholder : id -> Input id
elementPlaceholder id =
    init (Internal.Element id) []


init : Internal.InputType id Error -> List (Attribute id) -> Input id
init inputType attributes =
    Input (Tree.leaf (Internal.init inputType (unwrapAttrs attributes)))


{-| TODO
-}
mapIdentifier : (a -> b) -> Input a -> Input b
mapIdentifier func (Input tree) =
    Input (Tree.mapValues (Internal.mapIdentifier func) tree)


unwrapAttrs :
    List (Attribute id)
    -> List (Internal.Input id Error -> Internal.Input id Error)
unwrapAttrs =
    List.map (\(Attribute f) -> f)


{-| TODO
-}
type Attribute id
    = Attribute (Internal.Input id Error -> Internal.Input id Error)


{-| TODO
-}
name : String -> Attribute id
name str =
    Attribute (\input -> { input | name = str })


{-| TODO
-}
identifier : id -> Attribute id
identifier id =
    Attribute (\input -> { input | identifier = Just id })


{-| TODO
-}
value : Value.Value -> Attribute id
value (Value.Value inputValue) =
    Attribute (\input -> { input | value = inputValue })


{-| TODO
-}
required : Bool -> Attribute id
required bool =
    Attribute (\input -> { input | isRequired = bool })


{-| TODO
-}
label : String -> Attribute id
label str =
    Attribute (\input -> { input | label = Just str })


{-| TODO
-}
hint : String -> Attribute id
hint str =
    Attribute (\input -> { input | hint = Just str })


{-| TODO
-}
placeholder : String -> Attribute id
placeholder str =
    Attribute (\input -> { input | placeholder = Just str })


{-| TODO
-}
options : List ( String, Value.Value ) -> Attribute id
options values =
    Attribute
        (\input ->
            { input
                | options =
                    List.map (Tuple.mapSecond (\(Value.Value val) -> val))
                        values
            }
        )


{-| TODO
-}
min : Value.Value -> Attribute id
min (Value.Value val) =
    Attribute (\input -> { input | min = val })


{-| TODO
-}
max : Value.Value -> Attribute id
max (Value.Value val) =
    Attribute (\input -> { input | max = val })


{-| TODO
-}
inline : Bool -> Attribute id
inline bool =
    Attribute (\input -> { input | inline = bool })


{-| TODO
-}
noattr : Attribute id
noattr =
    Attribute identity


{-| TODO
-}
type HtmlAttribute id msg
    = HtmlAttribute (HtmlAttributes id msg -> HtmlAttributes id msg)


{-| TODO
-}
type alias HtmlAttributes id msg =
    { onChange : Maybe (Msg id -> msg)
    , addInputsButtonContent : Maybe id -> Html msg
    , removeInputsButtonContent : Maybe id -> Html msg
    , errorToHtml : Maybe id -> Error -> Html msg
    , elements : List ( id, Html msg )
    }


{-| TODO
-}
toHtml : List (HtmlAttribute id msg) -> Input id -> Html msg
toHtml attributes =
    toHtmlHelp
        (List.foldl (\(HtmlAttribute func) params -> func params)
            { onChange = Nothing
            , addInputsButtonContent = \_ -> Html.text "Add"
            , removeInputsButtonContent = \_ -> Html.text "Remove"
            , errorToHtml =
                \id err -> Html.text (errorToEnglish id err)
            , elements = []
            }
            attributes
        )
        []


{-| TODO
-}
onChange : (Msg id -> msg) -> HtmlAttribute id msg
onChange tagger =
    HtmlAttribute (\attrs -> { attrs | onChange = Just tagger })


{-| TODO
-}
addInputsButtonContent : (Maybe id -> Html msg) -> HtmlAttribute id msg
addInputsButtonContent func =
    HtmlAttribute (\attrs -> { attrs | addInputsButtonContent = func })


{-| TODO
-}
removeInputsButtonContent : (Maybe id -> Html msg) -> HtmlAttribute id msg
removeInputsButtonContent func =
    HtmlAttribute (\attrs -> { attrs | removeInputsButtonContent = func })


{-| TODO
-}
errorToHtml : (Maybe id -> Error -> Html msg) -> HtmlAttribute id msg
errorToHtml func =
    HtmlAttribute (\attrs -> { attrs | errorToHtml = func })


{-| TODO
-}
elementHtml : id -> Html msg -> HtmlAttribute id msg
elementHtml id html =
    HtmlAttribute
        (\attrs ->
            { attrs | elements = ( id, html ) :: attrs.elements }
        )


{-| TODO
-}
toHtmlHelp : HtmlAttributes id msg -> List Int -> Input id -> Html msg
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
                                fromTree >> toHtmlHelp attrs (path ++ [ idx ])
                            )

                inputLabel =
                    case input.label of
                        Just labelStr ->
                            Html.legend [] [ Html.text labelStr ]

                        Nothing ->
                            Html.text ""
            in
            Html.fieldset
                [ Attributes.id (identifierString input.name path)
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
                                fromTree
                                    >> templateHtml attrs
                                        (path ++ [ idx ])
                                        (List.length children /= (idx + 1))
                            )
            in
            Html.fieldset
                [ Attributes.id (identifierString input.name path) ]
                [ Html.div [] (legend input.label :: inputs)
                , addInputsButton attrs (Tree.value data |> .identifier) path
                ]

        Internal.Text ->
            inputToHtml attrs "text" path node []
                |> wrapInput attrs path node

        Internal.Email ->
            inputToHtml attrs "email" path node []
                |> wrapInput attrs path node

        Internal.Password ->
            inputToHtml attrs "password" path node []
                |> wrapInput attrs path node

        Internal.TextArea ->
            textAreaToHtml attrs path node
                |> wrapInput attrs path node

        Internal.Integer ->
            inputToHtml attrs "number" path node [ Attributes.step "1" ]
                |> wrapInput attrs path node

        Internal.Float ->
            inputToHtml attrs "number" path node [ Attributes.step "1" ]
                |> wrapInput attrs path node

        Internal.Date ->
            inputToHtml attrs "date" path node []
                |> wrapInput attrs path node

        Internal.Month ->
            inputToHtml attrs "month" path node []
                |> wrapInput attrs path node

        Internal.Select ->
            selectToHtml attrs path node
                |> wrapInput attrs path node

        Internal.Radio ->
            radioToHtml attrs path node
                |> wrapInput attrs path node

        Internal.Checkbox ->
            checkboxToHtml attrs path node
                |> wrapInput attrs path node

        Internal.Element elementId ->
            attrs.elements
                |> List.filter (Tuple.first >> (==) elementId)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault (Html.text "")


inputToHtml :
    HtmlAttributes id msg
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


textAreaToHtml : HtmlAttributes id msg -> List Int -> Input id -> Html msg
textAreaToHtml attrs path input =
    let
        valueStr =
            getValue input
                |> Value.toString
                |> Maybe.withDefault ""
    in
    Html.div
        [ Attributes.class "grow-wrap"
        , Attributes.attribute "data-replicated-value" valueStr
        ]
        [ Html.textarea
            (onInputChanged attrs path
                :: Attributes.value valueStr
                :: inputAttrs attrs path input
            )
            []
        ]


selectToHtml : HtmlAttributes id msg -> List Int -> Input id -> Html msg
selectToHtml attrs path input =
    let
        data =
            Tree.value (toTree input)
    in
    Html.select
        [ Attributes.id (identifierString data.name path)
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


radioToHtml : HtmlAttributes id msg -> List Int -> Input id -> Html msg
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
                        identifierString (data.name ++ String.fromInt index)
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


checkboxToHtml : HtmlAttributes id msg -> List Int -> Input id -> Html msg
checkboxToHtml attrs path input =
    Html.input
        (Attributes.type_ "checkbox"
            :: (getValue input
                    |> Value.toBool
                    |> Maybe.map Attributes.checked
                    |> Maybe.withDefault (Attributes.class "")
               )
            :: (case attrs.onChange of
                    Just tagger ->
                        Events.onCheck (InputChecked path >> tagger)

                    Nothing ->
                        Attributes.class ""
               )
            :: inputAttrs attrs path input
        )
        []


valueAttribute : (String -> Html.Attribute msg) -> Internal.Value.Value -> Html.Attribute msg
valueAttribute f inputValue =
    Internal.Value.toString inputValue
        |> Result.map f
        |> Result.withDefault (Attributes.class "")


inputAttrs : HtmlAttributes id msg -> List Int -> Input id -> List (Html.Attribute msg)
inputAttrs attrs path input =
    let
        data =
            Tree.value (toTree input)
    in
    [ Attributes.id (identifierString data.name path)
    , Attributes.required data.isRequired
    , Attributes.autocomplete False
    , Attributes.placeholder (Maybe.withDefault "" data.placeholder)
    , onInputFocused attrs path
    , onInputBlured attrs path
    , valueAttribute Attributes.min data.min
    , valueAttribute Attributes.max data.max
    ]


onInputChanged : HtmlAttributes id msg -> List Int -> Html.Attribute msg
onInputChanged attrs path =
    case attrs.onChange of
        Just tagger ->
            Events.onInput (InputChanged path >> tagger)

        Nothing ->
            Attributes.class ""


onInputFocused : HtmlAttributes id msg -> List Int -> Html.Attribute msg
onInputFocused attrs path =
    case attrs.onChange of
        Just tagger ->
            Events.onFocus (tagger (InputFocused path))

        Nothing ->
            Attributes.class ""


onInputBlured : HtmlAttributes id msg -> List Int -> Html.Attribute msg
onInputBlured attrs path =
    case attrs.onChange of
        Just tagger ->
            Events.onBlur (tagger (InputBlured path))

        Nothing ->
            Attributes.class ""


wrapInput : HtmlAttributes id msg -> List Int -> Input id -> Html msg -> Html msg
wrapInput attrs path input inputHtml =
    let
        data =
            Tree.value (toTree input)
    in
    Html.div
        [ Attributes.class "field"
        , Attributes.classList [ ( "required", data.isRequired ) ]
        ]
        [ Html.label
            [ Attributes.for (identifierString data.name path) ]
            [ Html.text (Maybe.withDefault data.name data.label) ]
        , Html.div
            [ Attributes.class "input-wrapper" ]
            [ inputHtml ]
        , case error input of
            Just message ->
                Html.p
                    [ Attributes.class "error" ]
                    [ attrs.errorToHtml (toId input) message ]

            Nothing ->
                case data.hint of
                    Just msg ->
                        Html.div
                            [ Attributes.class "hint" ]
                            [ Markdown.toHtml msg ]

                    Nothing ->
                        Html.text ""
        ]


templateHtml :
    HtmlAttributes id msg
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
                            (Decode.succeed ( tagger (InputsRemoved path), True ))

                    Nothing ->
                        Attributes.class ""
                ]
                [ attributes.removeInputsButtonContent (toId inputElement)
                ]

          else
            Html.text ""
        ]


addInputsButton : HtmlAttributes id msg -> Maybe id -> List Int -> Html msg
addInputsButton attrs id path =
    Html.button
        [ Attributes.class "add-fields"
        , case attrs.onChange of
            Just tagger ->
                Events.preventDefaultOn "click"
                    (Decode.succeed ( tagger (InputsAdded path), True ))

            Nothing ->
                Attributes.class ""
        ]
        [ attrs.addInputsButtonContent id
        ]


identifierString : String -> List Int -> String
identifierString nameStr path =
    String.join "-" (nameStr :: List.map String.fromInt path)


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
update : Msg id -> Input id -> Input id
update msg input =
    case msg of
        InputChanged path str ->
            updateAt path (updateInput str) input

        InputChecked path bool ->
            updateAt path (updateInputWithBool bool) input

        InputFocused path ->
            updateAt path resetStatus input

        InputBlured path ->
            updateAt path validate input

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
            fromTree (Tree.removeAt path (toTree input))


updateInput : String -> Tree (Internal.Input id Error) -> Tree (Internal.Input id Error)
updateInput string =
    Tree.updateValue (Internal.updateWithString string)


updateInputWithBool : Bool -> Tree (Internal.Input id Error) -> Tree (Internal.Input id Error)
updateInputWithBool bool =
    Tree.updateValue (Internal.update (Internal.Value.fromBool bool))


resetStatus : Tree (Internal.Input id Error) -> Tree (Internal.Input id Error)
resetStatus =
    Tree.updateValue Internal.resetStatus


{-| TODO
-}
type Error
    = TooLarge { value : Value.Value, max : Value.Value }
    | TooSmall { value : Value.Value, min : Value.Value }
    | NotInRange
        { value : Value.Value
        , min : Value.Value
        , max : Value.Value
        }
    | IsBlank


{-| TODO
-}
error : Input id -> Maybe Error
error input =
    let
        { status } =
            Tree.value (toTree input)
    in
    case status of
        Internal.WithError err ->
            Just err

        _ ->
            Nothing


{-| TODO
-}
check : Internal.Input id Error -> Result Error Value.Value
check input =
    checkRequired input
        |> Result.andThen (\_ -> checkInRange input)


checkRequired : Internal.Input id Error -> Result Error Value.Value
checkRequired input =
    if input.isRequired && Internal.Value.isBlank input.value then
        Err IsBlank

    else
        Ok (Value.Value input.value)


checkInRange : Internal.Input id Error -> Result Error Value.Value
checkInRange input =
    let
        actual =
            Value.Value input.value
    in
    case
        ( Internal.Value.compare input.value input.min
        , Internal.Value.compare input.value input.max
        )
    of
        ( Just LT, Just _ ) ->
            Err
                (NotInRange
                    { value = actual
                    , min = Value.Value input.min
                    , max = Value.Value input.max
                    }
                )

        ( Just _, Just GT ) ->
            Err
                (NotInRange
                    { value = actual
                    , min = Value.Value input.min
                    , max = Value.Value input.max
                    }
                )

        ( Just LT, Nothing ) ->
            Err
                (TooSmall
                    { value = actual
                    , min = Value.Value input.min
                    }
                )

        ( Nothing, Just GT ) ->
            Err
                (TooLarge
                    { value = actual
                    , max = Value.Value input.max
                    }
                )

        _ ->
            Ok actual


{-| TODO
-}
errorToEnglish : id -> Error -> String
errorToEnglish _ err =
    let
        toString =
            Value.toString
                >> Maybe.withDefault ""
    in
    case err of
        TooLarge data ->
            "Should be lesser than " ++ toString data.max

        TooSmall data ->
            "Should be greater than " ++ toString data.min

        NotInRange data ->
            "Should be between " ++ toString data.min ++ " and " ++ toString data.max

        IsBlank ->
            "Should be provided"


{-| -}
getValue : Input id -> Value.Value
getValue (Input tree) =
    Tree.value tree |> .value |> Value.Value


{-| -}
fromTree : Tree (Internal.Input id Error) -> Input id
fromTree =
    Input


{-| -}
toTree : Input id -> Tree (Internal.Input id Error)
toTree (Input tree) =
    tree


toId : Input id -> Maybe id
toId (Input input) =
    Tree.value input |> .identifier


{-| TODO
-}
clear : Input id -> Input id
clear =
    map (Tree.updateValue (Internal.update Internal.Value.blank))


validate : Tree (Internal.Input id Error) -> Tree (Internal.Input id Error)
validate =
    Tree.updateValue
        (\node ->
            case check node of
                Ok _ ->
                    { node | status = Internal.Valid }

                Err err ->
                    { node | status = Internal.WithError err }
        )


map :
    (Tree (Internal.Input id Error) -> Tree (Internal.Input id Error))
    -> Input id
    -> Input id
map func input =
    fromTree (Tree.map func (toTree input))


updateAt :
    List Int
    -> (Tree (Internal.Input id Error) -> Tree (Internal.Input id Error))
    -> Input id
    -> Input id
updateAt path func input =
    fromTree (Tree.updateAt path func (toTree input))


{-| TODO
-}
toJSON : Input id -> Encode.Value
toJSON input =
    Encode.object (encodeHelp input [])


encodeHelp :
    Input id
    -> List ( String, Encode.Value )
    -> List ( String, Encode.Value )
encodeHelp inputElement acc =
    let
        tree =
            toTree inputElement

        input =
            Tree.value tree

        children =
            Tree.children tree
    in
    case input.inputType of
        Internal.Group ->
            List.foldl (encodeHelp << fromTree) acc children

        Internal.Repeatable _ ->
            ( input.name
            , Encode.list
                (\e -> Encode.object (encodeHelp (fromTree e) []))
                children
            )
                :: acc

        _ ->
            ( input.name, Internal.Value.encode input.value ) :: acc


valueDecoder : Decode.Decoder Internal.Value.Value
valueDecoder =
    Decode.oneOf
        [ Decode.map Internal.Value.fromBool Decode.bool
        , Decode.map Internal.Value.fromString Decode.string
        , Decode.map Internal.Value.fromInt Decode.int
        , Decode.map Internal.Value.fromFloat Decode.float
        ]
