module StringFormatTest exposing (suite)

import Expect
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html.Attributes as Attrs
import Support.Interaction as Interaction exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag)


suite : Test
suite =
    describe "formattedString integration tests"
        [ describe "basic formatting"
            [ test "formats simple credit card number updating cursor position" <|
                \_ ->
                    input "1234567890123456"
                        |> Interaction.init
                            (Parse.formattedString
                                "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}"
                            )
                        |> fillInput "credit-card" "1234567890123456"
                        |> Expect.all
                            [ \{ result } -> Expect.equal result (Ok "1234 5678 9012 3456")
                            , \{ field } ->
                                field
                                    |> Field.toHtml (always never)
                                    |> Query.fromHtml
                                    |> Query.find [ tag "input" ]
                                    |> Query.has [ attribute (Attrs.value "1234 5678 9012 3456") ]
                            ]
            , test "handles partial input" <|
                \_ ->
                    input ""
                        |> Interaction.init
                            (Parse.formattedString
                                "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}"
                            )
                        |> fillInput "credit-card" "12345678"
                        |> Expect.all
                            [ \{ result } ->
                                case result of
                                    Err _ ->
                                        Expect.pass

                                    Ok _ ->
                                        Expect.fail "Expected error for incomplete pattern"
                            , \{ field } ->
                                field
                                    |> Field.toHtml (always never)
                                    |> Query.fromHtml
                                    |> Query.find [ tag "input" ]
                                    |> Query.has [ attribute (Attrs.value "1234 5678") ]
                            ]
            ]
        , describe "cursor position tracking"
            [ test "insert respecting format" <|
                \_ ->
                    input ""
                        |> Interaction.init
                            (Parse.formattedString
                                "{d}{d}{d}{d} {d}{d}{d}{d}"
                            )
                        |> fillInput "credit-card" "1234 5678"
                        |> Expect.all
                            [ \{ result } -> Expect.equal result (Ok "1234 5678")
                            , \{ field } ->
                                field
                                    |> Field.toHtml (always never)
                                    |> Query.fromHtml
                                    |> Query.find [ tag "input" ]
                                    |> Query.has [ attribute (Attrs.value "1234 5678") ]
                            ]
            , test "cursor position with partial input and space insertion" <|
                \_ ->
                    input ""
                        |> Interaction.init
                            (Parse.formattedString
                                "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}"
                            )
                        |> fillInput "credit-card" "12345"
                        |> Expect.all
                            [ \{ result } ->
                                case result of
                                    Err _ ->
                                        Expect.pass

                                    Ok _ ->
                                        Expect.fail "Expected error for incomplete pattern"
                            , \{ field } ->
                                field
                                    |> Field.toHtml (always never)
                                    |> Query.fromHtml
                                    |> Query.find [ tag "input" ]
                                    |> Query.has [ attribute (Attrs.value "1234 5") ]
                            ]
            ]
        , describe "handling spaces in input"
            [ test "input with spaces that match mask literals" <|
                \_ ->
                    input ""
                        |> Interaction.init
                            (Parse.formattedString
                                "{d}{d}{d}{d} {d}{d}{d}{d}"
                            )
                        |> fillInput "credit-card" "1234 5678"
                        |> Expect.all
                            [ \{ result } -> Expect.equal result (Ok "1234 5678")
                            , \{ field } ->
                                field
                                    |> Field.toHtml (always never)
                                    |> Query.fromHtml
                                    |> Query.find [ tag "input" ]
                                    |> Query.has [ attribute (Attrs.value "1234 5678") ]
                            ]
            , test "input with extra digits should be formatted correctly 2" <|
                \_ ->
                    input ""
                        |> Interaction.init
                            (Parse.formattedString
                                "{d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d} {d}{d}{d}{d}"
                            )
                        |> fillInput "credit-card" "19090 9090 9090 9090"
                        |> Expect.all
                            [ \{ result } -> Expect.equal result (Ok "1909 0909 0909 0909")
                            , \{ field } ->
                                field
                                    |> Field.toHtml (always never)
                                    |> Query.fromHtml
                                    |> Query.find [ tag "input" ]
                                    |> Query.has [ attribute (Attrs.value "1909 0909 0909 0909") ]
                            ]
            , test "input with extra digits should be formatted correctly" <|
                \_ ->
                    input ""
                        |> Interaction.init
                            (Parse.formattedString
                                "{d}{d} {d}{d}"
                            )
                        |> fillInput "credit-card" "190 99"
                        |> Expect.all
                            [ \{ result } -> Expect.equal result (Ok "19 09")
                            , \{ field } ->
                                field
                                    |> Field.toHtml (always never)
                                    |> Query.fromHtml
                                    |> Query.find [ tag "input" ]
                                    |> Query.has [ attribute (Attrs.value "19 09") ]
                            ]
            , test "input with space should remove extra characters and format correctly" <|
                \_ ->
                    input ""
                        |> Interaction.init (Parse.formattedString "{d}{d} {d}{d}")
                        |> fillInput "credit-card" "2 3"
                        |> Expect.all
                            [ \{ result } ->
                                case result of
                                    Err _ ->
                                        Expect.pass

                                    Ok _ ->
                                        Expect.fail "Expected error for incomplete pattern"
                            , \{ field } ->
                                field
                                    |> Field.toHtml (always never)
                                    |> Query.fromHtml
                                    |> Query.find [ tag "input" ]
                                    |> Query.has [ attribute (Attrs.value "23") ]
                            ]
            , test "input with extra spaces removes them and adjusts cursor position" <|
                \_ ->
                    input ""
                        |> Interaction.init
                            (Parse.formattedString
                                "{d}{d}{d}{d} {d}{d}"
                            )
                        |> fillInput "credit-card" "123 4 56"
                        |> Expect.all
                            [ \{ result } -> Expect.equal result (Ok "1234 56")
                            , \{ field } ->
                                field
                                    |> Field.toHtml (always never)
                                    |> Query.fromHtml
                                    |> Query.find [ tag "input" ]
                                    |> Query.has [ attribute (Attrs.value "1234 56") ]
                            ]
            ]
        , describe "edge cases"
            [ test "empty input" <|
                \_ ->
                    input ""
                        |> Interaction.init
                            (Parse.formattedString
                                "{d}{d}{d}{d} {d}{d}{d}{d}"
                            )
                        |> fillInput "credit-card" ""
                        |> Expect.all
                            [ \{ result } ->
                                case result of
                                    Err _ ->
                                        Expect.pass

                                    Ok _ ->
                                        Expect.fail "Expected error for empty input"
                            , \{ field } ->
                                field
                                    |> Field.toHtml (always never)
                                    |> Query.fromHtml
                                    |> Query.find [ tag "input" ]
                                    |> Query.has [ attribute (Attrs.value "") ]
                            ]
            , test "input longer than mask" <|
                \_ ->
                    input ""
                        |> Interaction.init (Parse.formattedString "{d}{d}{d}{d}")
                        |> fillInput "credit-card" "123456789"
                        |> Expect.all
                            [ \{ result } -> Expect.equal result (Ok "1234")
                            , \{ field } ->
                                field
                                    |> Field.toHtml (always never)
                                    |> Query.fromHtml
                                    |> Query.find [ tag "input" ]
                                    |> Query.has [ attribute (Attrs.value "1234") ]
                            ]
            ]
        ]


input : String -> Field ()
input initialValue =
    Field.text
        [ Field.name "credit-card"
        , Field.value (Value.string initialValue)
        ]
