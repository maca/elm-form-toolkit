module FormToolkit.Error exposing (Error(..))

import Internal.Value exposing (Value)
import Json.Decode as Decode


type Error
    = TooLarge Value
    | TooSmall Value
    | NotInRange ( Value, Value )
    | NotInOptions
    | IsBlank
    | InputNotFound
    | NotString
    | NotInt
    | NotFloat
    | NotBool
    | NotPosix
    | DecodeError Decode.Error
