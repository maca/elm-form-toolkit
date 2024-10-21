module InputTest exposing (suite)

import Expect
import FormToolkit.Decode as Decode exposing (Error(..))
import FormToolkit.Input as Input
import FormToolkit.Value as Value
import Support.ExampleInputs exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Input"
        [ let
            input =
                Input.group []
                    [ Input.group []
                        [ Input.text
                            [ Input.identifier "NestedInput"
                            , Input.value (Value.string "Value")
                            ]
                        , Input.text
                            [ Input.identifier "NestedInput2"
                            , Input.value (Value.string "Value2")
                            ]
                        ]
                    ]
          in
          describe "update attributes"
            [ test "is success" <|
                \_ ->
                    input
                        |> Input.updateAttributes "NestedInput"
                            [ Input.value (Value.string "Updated value") ]
                        |> Result.mapError List.singleton
                        |> Result.andThen
                            (Decode.decode
                                (Decode.map2 Tuple.pair
                                    (Decode.field "NestedInput" Decode.string)
                                    (Decode.field "NestedInput2" Decode.string)
                                )
                            )
                        |> Expect.equal
                            (Ok ( "Updated value", "Value2" ))
            , test "fails when no matching id" <|
                \_ ->
                    input
                        |> Input.updateAttributes "NonExisting"
                            [ Input.value (Value.string "Value2") ]
                        |> Expect.equal
                            (Err (InputNotFound "NonExisting"))
            ]
        ]
