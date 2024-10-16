module Internal.Input exposing
    ( Tree, Input(..), Attrs, InputType(..), Status(..)
    , blur, focus, init, isBlank, map, root
    , updateValue
    , Msg(..), inputChanged
    )

{-|

@docs Tree, Input, Attrs, InputType, Status
@docs blur, focus, init, isBlank, map, root
@docs updateValue
@docs Msg, inputChanged

-}

import Array
import Internal.Value exposing (Value)
import List.Extra as List
import RoseTree.Tree as Tree exposing (Tree)


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
    | Repeatable (Tree id val err)


type alias Attrs id val err =
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


type alias Tree id val err =
    Tree.Tree (Attrs id val err)


type Input id val err
    = Input (Tree id val err)


root : Attrs id val err
root =
    init Group []


init : InputType id val err -> List (Attrs id val err -> Attrs id val err) -> Attrs id val err
init inputType =
    List.foldl (\f i -> f i)
        { inputType = inputType
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


updateValue : Value val -> Attrs id val err -> Attrs id val err
updateValue value input =
    { input | value = value, errors = [] }


getChoice : String -> Attrs id val err -> Value val
getChoice str { options } =
    case String.toInt str of
        Just idx ->
            Array.fromList options
                |> Array.get idx
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Internal.Value.blank

        Nothing ->
            Internal.Value.blank


focus : Attrs id val err -> Attrs id val err
focus input =
    { input | status = Focused }


blur : Attrs id val err -> Attrs id val err
blur input =
    { input | status = Touched }


isBlank : Attrs id val err -> Bool
isBlank { value, inputType } =
    case inputType of
        Group ->
            False

        Repeatable _ ->
            False

        _ ->
            Internal.Value.isBlank value


map : (a -> b) -> (Value val1 -> Value val2) -> (err1 -> err2) -> Attrs a val1 err1 -> Attrs b val2 err2
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
mapInputType func errToErr valToVal inputType =
    case inputType of
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


type Msg id val
    = InputChanged (List Int) (Value val)
    | InputFocused (List Int)
    | InputBlured (List Int)
    | InputsAdded (List Int)
    | InputsRemoved (List Int)


inputChanged : Input id val err -> List Int -> String -> Msg id val
inputChanged (Input tree) path str =
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
