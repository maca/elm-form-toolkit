module InputTest exposing (suite)

import Expect
import FormToolkit.Decode as Decode
import FormToolkit.Field as Input
import FormToolkit.Value as Value
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
                        |> Input.updateBy "NestedInput"
                            (Input.updateAttribute
                                (Input.value (Value.string "Updated value"))
                            )
                        |> Maybe.map
                            (Decode.decode
                                (Decode.map2 Tuple.pair
                                    (Decode.field "NestedInput" Decode.string)
                                    (Decode.field "NestedInput2" Decode.string)
                                )
                            )
                        |> Expect.equal (Just (Ok ( "Updated value", "Value2" )))
            , test "it preserves identifier" <|
                \_ ->
                    input
                        |> Input.updateBy "NestedInput"
                            (Input.updateAttribute (Input.identifier "OtherInput"))
                        |> Maybe.map
                            (Decode.decode
                                (Decode.map2 Tuple.pair
                                    (Decode.field "NestedInput" Decode.string)
                                    (Decode.field "NestedInput2" Decode.string)
                                )
                            )
                        |> Expect.equal (Just (Ok ( "Value", "Value2" )))
            , test "fails when no matching id" <|
                \_ ->
                    input
                        |> Input.updateBy "NotExisting"
                            (Input.updateAttribute
                                (Input.value (Value.string "Updated value"))
                            )
                        |> Expect.equal Nothing
            ]
        ]
