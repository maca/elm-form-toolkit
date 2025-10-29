module Internal.Field exposing (Attributes, FieldType(..), Status(..))

{-|

@docs Attributes, FieldType, Status

-}

import Internal.Utils
import Internal.Value exposing (Value)
import RoseTree.Tree as Tree


type Status
    = Pristine
    | Focused
    | Touched


type FieldType id err
    = Text
    | TextArea
    | Email
    | Password
    | StrictAutocomplete
    | Integer
    | Float
    | Month
    | Date
    | LocalDatetime
    | Select
    | Radio
    | Checkbox
    | Group
    | Repeatable (Tree.Tree (Attributes id (FieldType id err) err))


type alias Attributes id fieldType err =
    { inputType : fieldType
    , name : Maybe String
    , value : Value
    , isRequired : Bool
    , label : Maybe String
    , placeholder : Maybe String
    , hint : Maybe String
    , min : Value
    , max : Value
    , step : Value
    , autogrow : Bool
    , options : List ( String, Value )
    , identifier : Maybe id
    , status : Status
    , repeatableMin : Int
    , repeatableMax : Maybe Int
    , addFieldsButtonCopy : String
    , removeFieldsButtonCopy : String
    , errors : err
    , classList : List String
    , selectionStart : Int
    , selectionEnd : Int
    , disabled : Bool
    , hidden : Bool
    , pattern : List Internal.Utils.MaskToken
    }
