module Internal.Input exposing
    ( Input, Attributes, InputType(..), Status(..)
    , blur, focus, init, isBlank, map
    , updateAttributes, updateValue
    , Msg(..), inputChanged
    , identifier, inputType, max, min, name, value
    , label, hint, placeholder
    , isGroup, isRequired
    , errors, setErrors
    , inputIdString
    , isAutocompleteable, options
    )

{-|

@docs Input, Attributes, InputType, Status
@docs blur, focus, init, isBlank, map
@docs updateAttributes, updateValue
@docs Msg, inputChanged
@docs identifier, inputType, max, min, name, value
@docs label, hint, placeholder
@docs isGroup, isRequired
@docs errors, setErrors
@docs inputIdString

-}

import Array
import Dict
import Internal.Value exposing (Value)
import List.Extra
import RoseTree.Tree as Tree


type Status
    = Pristine
    | Focused
    | Touched


type InputType id val err
    = Text
    | TextArea
    | Password
    | Email
    | Integer
    | Float
    | Month
    | Date
    | Select
    | Radio
    | Checkbox
    | Group
    | Repeatable (Input id val err)


type alias Attributes id val err =
    { inputType : InputType id val err
    , name : Maybe String
    , value : Value val
    , isRequired : Bool
    , label : Maybe String
    , placeholder : Maybe String
    , hint : Maybe String
    , min : Value val
    , max : Value val
    , options : List ( String, Value val )
    , identifier : Maybe id
    , status : Status
    , repeatableMin : Int
    , repeatableMax : Maybe Int
    , addInputsButtonCopy : String
    , removeInputsButtonCopy : String
    , errors : List err
    }


type alias Input id val err =
    Tree.Tree (Attributes id val err)


init : InputType id val err -> List (Attributes id val err -> Attributes id val err) -> Attributes id val err
init inputType_ =
    List.foldl (<|)
        { inputType = inputType_
        , name = Nothing
        , label = Nothing
        , hint = Nothing
        , placeholder = Nothing
        , value = Internal.Value.blank
        , min = Internal.Value.blank
        , max = Internal.Value.blank
        , isRequired = False
        , options = []
        , identifier = Nothing
        , status = Pristine
        , repeatableMin = 1
        , repeatableMax = Nothing
        , addInputsButtonCopy = "Add"
        , removeInputsButtonCopy = "Remove"
        , errors = []
        }


updateAttributes :
    List (Attributes id val err -> Attributes id val err)
    -> Input id val err
    -> Input id val err
updateAttributes attrList =
    Tree.updateValue
        (\attrs ->
            let
                updatedAttrs =
                    List.foldl (<|) attrs attrList
            in
            { updatedAttrs | identifier = attrs.identifier }
        )


updateValue : Value val -> Input id val err -> Input id val err
updateValue val =
    Tree.updateValue (\attrs -> { attrs | value = val, errors = [] })


getChoice : String -> Attributes id val err -> Value val
getChoice str attributes =
    case String.toInt str of
        Just idx ->
            Array.fromList attributes.options
                |> Array.get idx
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Internal.Value.blank

        Nothing ->
            Internal.Value.blank


focus : Attributes id val err -> Attributes id val err
focus input =
    { input | status = Focused }


blur : Attributes id val err -> Attributes id val err
blur input =
    { input | status = Touched }


isBlank : Input id val err -> Bool
isBlank input =
    case Tree.value input |> .inputType of
        Group ->
            False

        Repeatable _ ->
            False

        _ ->
            Internal.Value.isBlank (value input)


identifier : Input id val err -> Maybe id
identifier input =
    Tree.value input |> .identifier


value : Input id val err -> Value val
value input =
    Tree.value input |> .value


name : Input id val err -> Maybe String
name input =
    Tree.value input |> .name


placeholder : Input id val err -> Maybe String
placeholder input =
    Tree.value input |> .placeholder


label : Input id val err -> Maybe String
label input =
    Tree.value input |> .label


hint : Input id val err -> Maybe String
hint input =
    Tree.value input |> .hint


inputType : Input id val err -> InputType id val err
inputType input =
    Tree.value input |> .inputType


min : Input id val err -> Value val
min input =
    Tree.value input |> .min


max : Input id val err -> Value val
max input =
    Tree.value input |> .max


options : Input id val err -> List ( String, Value val )
options input =
    Tree.value input |> .options


isRequired : Input id val err -> Bool
isRequired input =
    Tree.value input |> .isRequired


isAutocompleteable : Input id val err -> Bool
isAutocompleteable input =
    case inputType input of
        Text ->
            not (List.isEmpty (options input))

        _ ->
            False


isGroup : Input id val err -> Bool
isGroup input =
    case Tree.value input |> .inputType of
        Group ->
            True

        Repeatable _ ->
            True

        _ ->
            False


errors : Input id val err -> List err
errors tree =
    case Tree.children tree of
        [] ->
            Tree.value tree |> .errors

        children ->
            (Tree.value tree |> .errors)
                :: List.map errors children
                |> List.concat


setErrors : List err -> Input id val err -> Input id val err
setErrors error =
    Tree.updateValue
        (\input ->
            { input
                | errors = List.Extra.unique (error ++ input.errors)
            }
        )


map : (a -> b) -> (Value val1 -> Value val2) -> (err1 -> err2) -> Attributes a val1 err1 -> Attributes b val2 err2
map func valToVal errToErr input =
    { inputType = mapInputType func errToErr valToVal input.inputType
    , name = input.name
    , value = valToVal input.value
    , isRequired = input.isRequired
    , label = input.label
    , placeholder = input.placeholder
    , hint = input.hint
    , min = valToVal input.min
    , max = valToVal input.max
    , options = List.map (Tuple.mapSecond valToVal) input.options
    , identifier = Maybe.map func input.identifier
    , status = input.status
    , repeatableMin = input.repeatableMin
    , repeatableMax = input.repeatableMax
    , addInputsButtonCopy = input.addInputsButtonCopy
    , removeInputsButtonCopy = input.removeInputsButtonCopy
    , errors = List.map errToErr input.errors
    }


mapInputType : (a -> b) -> (err1 -> err2) -> (Value val1 -> Value val2) -> InputType a val1 err1 -> InputType b val2 err2
mapInputType func errToErr valToVal inputType_ =
    case inputType_ of
        Repeatable tree ->
            Repeatable (Tree.mapValues (map func valToVal errToErr) tree)

        Text ->
            Text

        TextArea ->
            TextArea

        Password ->
            Password

        Email ->
            Email

        Integer ->
            Integer

        Float ->
            Float

        Month ->
            Month

        Date ->
            Date

        Select ->
            Select

        Radio ->
            Radio

        Checkbox ->
            Checkbox

        Group ->
            Group


inputIdString : Input id val err -> String
inputIdString input =
    name input
        |> Maybe.withDefault
            (inputType input |> inputTypeToString)


inputTypeToString : InputType id val err -> String
inputTypeToString type_ =
    case type_ of
        Text ->
            "text"

        TextArea ->
            "textarea"

        Password ->
            "password"

        Email ->
            "email"

        Integer ->
            "integer"

        Float ->
            "float"

        Month ->
            "month"

        Date ->
            "date"

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


type Msg id val
    = InputChanged (List Int) (Value val)
    | InputFocused (List Int)
    | InputBlured (List Int)
    | InputsAdded (List Int)
    | InputsRemoved (List Int)


inputChanged : Input id val err -> List Int -> String -> Msg id val
inputChanged tree path str =
    let
        unwrappedInput =
            Tree.value tree
    in
    InputChanged path <|
        case unwrappedInput.inputType of
            Text ->
                Internal.Value.fromString str

            TextArea ->
                Internal.Value.fromString str

            Password ->
                Internal.Value.fromString str

            Email ->
                Internal.Value.fromString str

            Integer ->
                Internal.Value.intFromString str

            Float ->
                Internal.Value.floatFromString str

            Month ->
                Internal.Value.monthFromString str

            Date ->
                Internal.Value.dateFromString str

            Select ->
                getChoice str unwrappedInput

            Radio ->
                getChoice str unwrappedInput

            Checkbox ->
                Internal.Value.blank

            Group ->
                Internal.Value.blank

            Repeatable _ ->
                Internal.Value.blank
