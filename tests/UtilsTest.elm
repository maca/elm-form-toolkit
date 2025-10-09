module UtilsTest exposing (suite)

import Expect
import FormToolkit.Utils exposing (formatMask)
import Test exposing (..)


suite : Test
suite =
    describe "formatMask"
        [ describe "basic formatting"
            [ test "formats simple credit card number updating cursor position" <|
                \_ ->
                    formatMask
                        { mask = "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}"
                        , input = "1234567890123456"
                        , cursorPosition = 16
                        }
                        |> Expect.equal
                            { formatted = "1234 5678 9012 3456"
                            , cursorPosition = 19
                            }
            , test "handles partial input" <|
                \_ ->
                    formatMask
                        { mask = "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}"
                        , input = "12345678"
                        , cursorPosition = 8
                        }
                        |> Expect.equal
                            { formatted = "1234 5678"
                            , cursorPosition = 9
                            }
            ]
        , describe "cursor position tracking"
            [ test "insert respecting format" <|
                \_ ->
                    formatMask
                        { mask = "{d}{d}{d}{d} {d}{d}{d}{d}"
                        , input = "1234 5678"
                        , cursorPosition = 6
                        }
                        |> Expect.equal
                            { formatted = "1234 5678"
                            , cursorPosition = 6
                            }

            -- , Test.only <|
            , test "cursor position with partial input and space insertion" <|
                \_ ->
                    formatMask
                        { mask = "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}"
                        , input = "12345"
                        , cursorPosition = 5
                        }
                        |> Expect.equal
                            { formatted = "1234 5"
                            , cursorPosition = 6
                            }
            ]
        , describe "handling spaces in input"
            [ test "input with spaces that match mask literals" <|
                \_ ->
                    formatMask
                        { mask = "{d}{d}{d}{d} {d}{d}{d}{d}"
                        , input = "1234 5678"
                        , cursorPosition = 4
                        }
                        |> Expect.equal
                            { formatted = "1234 5678"
                            , cursorPosition = 4
                            }
            , test "input with extra digits should be formatted correctly 2" <|
                \_ ->
                    formatMask
                        { mask = "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}"
                        , input = "19090 9090 9090 9090"
                        , cursorPosition = 1
                        }
                        |> Expect.equal
                            { formatted = "1909 0909 0909 0909"
                            , cursorPosition = 1
                            }
            , test "input with extra digits should be formatted correctly" <|
                \_ ->
                    formatMask
                        { mask = "{d}{d} {d}{d}"
                        , input = "190 99"
                        , cursorPosition = 1
                        }
                        |> Expect.equal
                            { formatted = "19 09"
                            , cursorPosition = 1
                            }
            , test "input with space should remove extra characters and format correctly" <|
                \_ ->
                    formatMask
                        { mask = "{d}{d} {d}{d}"
                        , input = "2 3"
                        , cursorPosition = 0
                        }
                        |> Expect.equal
                            { formatted = "23"
                            , cursorPosition = 0
                            }

            -- , Test.only <|
            , test "input with extra spaces removes them and adjusts cursor position" <|
                \_ ->
                    formatMask
                        { mask = "{d}{d}{d}{d} {d}{d}"
                        , input = "123 4 56"
                        , cursorPosition = 4
                        }
                        |> Expect.equal
                            { formatted = "1234 56"
                            , cursorPosition = 3
                            }
            ]
        , describe "edge cases"
            [ test "empty input" <|
                \_ ->
                    formatMask
                        { mask = "{d}{d}{d}{d} {d}{d}{d}{d}"
                        , input = ""
                        , cursorPosition = 0
                        }
                        |> Expect.equal
                            { formatted = ""
                            , cursorPosition = 0
                            }
            , test "cursor beyond input length" <|
                \_ ->
                    formatMask
                        { mask = "{d}{d}{d}{d} {d}{d}{d}{d}"
                        , input = "1234"
                        , cursorPosition = 10
                        }
                        |> Expect.equal
                            { formatted = "1234"
                            , cursorPosition = 4
                            }
            , test "input longer than mask" <|
                \_ ->
                    formatMask
                        { mask = "{d}{d}{d}{d}"
                        , input = "123456789"
                        , cursorPosition = 6
                        }
                        |> Expect.equal
                            { formatted = "1234"
                            , cursorPosition = 4
                            }
            ]
        ]
