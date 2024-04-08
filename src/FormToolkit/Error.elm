module FormToolkit.Error exposing (Error(..))

{-|

@docs Error

-}

import Internal.Value exposing (Value)


{-| TODO
-}
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
