module Support.Interaction exposing
    ( Interaction
    , blur
    , check
    , clickButton
    , fillInput
    , fillInputWithIndex
    , init
    , interact
    , select
    )

import FormToolkit.Error exposing (Error(..))
import FormToolkit.Field as Field exposing (Field, Msg)
import FormToolkit.Parse as Parse
import FormToolkit.View as View
import Html.Attributes exposing (name)
import Json.Encode
import RoseTree.Tree as Tree
import Test.Html.Event as Event
import Test.Html.Query as Query exposing (find, findAll, index)
import Test.Html.Selector exposing (attribute, containing, tag, text)


type alias Interaction id a =
    { field : Field id
    , decoder : Parse.Parser id a
    , result : Result (Error id) a
    }


fillInputWithIndex : Int -> String -> String -> Interaction id a -> Interaction id a
fillInputWithIndex idx inputName inputText =
    interact (findAll [ attribute (name inputName) ] >> index idx) (Event.input inputText)


clickButton : String -> Interaction id a -> Interaction id a
clickButton buttonText =
    interact (find [ tag "button", containing [ text buttonText ] ]) Event.click


blur : String -> Interaction id a -> Interaction id a
blur inputName =
    interact (findInput inputName) Event.blur


fillInput : String -> String -> Interaction id a -> Interaction id a
fillInput inputName inputText =
    interact (findInput inputName) (Event.input inputText)


check : String -> Bool -> Interaction id a -> Interaction id a
check inputName checked =
    interact (findInput inputName) (Event.check checked)


select : String -> String -> Interaction id a -> Interaction id a
select selectName optionLabel interaction =
    let
        (Field.Field tree) =
            interaction.field

        -- Find the field with the matching name and extract its options
        options =
            Tree.foldl
                (\node acc ->
                    let
                        attrs =
                            Tree.value node
                    in
                    if attrs.name == Just selectName then
                        attrs.options

                    else
                        acc
                )
                []
                tree

        -- Find the index of the option with the matching label
        optionIndex =
            options
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, ( label, _ ) ) -> label == optionLabel)
                |> List.head
                |> Maybe.map (Tuple.first >> String.fromInt)
                |> Maybe.withDefault "0"
    in
    interact
        (find [ tag "select", attribute (name selectName) ])
        (Event.input optionIndex)
        interaction


findInput : String -> Query.Single msg -> Query.Single msg
findInput inputName =
    find [ attribute (name inputName) ]


init : Parse.Parser id a -> Field id -> Interaction id a
init decoder input =
    { field = input
    , decoder = decoder
    , result = Err (CustomError Nothing "Not modified")
    }


interact :
    (Query.Single (Msg id) -> Query.Single (Msg id))
    -> ( String, Json.Encode.Value )
    -> Interaction id a
    -> Interaction id a
interact matcher event actions =
    let
        query =
            actions.field
                |> View.fromField identity
                |> View.toHtml
                |> Query.fromHtml

        ( field, result ) =
            matcher query
                |> Event.simulate event
                |> Event.toResult
                |> Result.map
                    (\msg ->
                        Parse.parseUpdate actions.decoder msg actions.field
                    )
                |> Result.mapError (CustomError Nothing)
                |> Result.withDefault ( actions.field, actions.result )
    in
    { actions | result = result, field = field }
