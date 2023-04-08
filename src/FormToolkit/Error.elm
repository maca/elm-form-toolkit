module FormToolkit.Error exposing (Error(..))

import Internal.Value exposing (Value)


type Error
    = TooLarge Value
    | TooSmall Value
    | NotInRange ( Value, Value )
    | NotInOptions
    | IsBlank
    | NotString
    | NotInt
    | NotFloat
    | NotBool
    | NotPosix
