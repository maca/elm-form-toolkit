module Support.Interaction exposing
    ( Interaction
    , blur
    , clickButton
    , fillInput
    , fillInputWithIndex
    , init
    , interact
    )

import FormToolkit.Field as Input
import FormToolkit.Parse as Decode
import FormToolkit.View as View
import Html.Attributes exposing (name)
import Json.Encode
import Test.Html.Event as Event
import Test.Html.Query as Query exposing (find, findAll, index)
import Test.Html.Selector exposing (attribute, containing, tag, text)


type alias Interaction id val a =
    { input : Input.Field id val
    , decoder : Decode.Parser id val a
    , result : Result (List (Decode.Error id val)) a
    }


fillInputWithIndex : Int -> String -> String -> Interaction id val a -> Interaction id val a
fillInputWithIndex idx inputName inputText =
    interact (findAll [ attribute (name inputName) ] >> index idx) (Event.input inputText)


clickButton : String -> Interaction id val a -> Interaction id val a
clickButton buttonText =
    interact (find [ tag "button", containing [ text buttonText ] ]) Event.click


blur : String -> Interaction id val a -> Interaction id val a
blur inputName =
    interact (findInput inputName) Event.blur


fillInput : String -> String -> Interaction id val a -> Interaction id val a
fillInput inputName inputText =
    interact (findInput inputName) (Event.input inputText)


findInput : String -> Query.Single msg -> Query.Single msg
findInput inputName =
    find [ attribute (name inputName) ]


init : Decode.Parser id val a -> Input.Field id val -> Interaction id val a
init decoder input =
    { input = input
    , decoder = decoder
    , result = Err [ Decode.CustomError Nothing "Not modified" ]
    }


interact :
    (Query.Single (Input.Msg id val) -> Query.Single (Input.Msg id val))
    -> ( String, Json.Encode.Value )
    -> Interaction id val a
    -> Interaction id val a
interact matcher event actions =
    let
        query =
            actions.input
                |> View.fromField identity
                |> View.toHtml
                |> Query.fromHtml

        ( input, result ) =
            matcher query
                |> Event.simulate event
                |> Event.toResult
                |> Result.map (\msg -> Input.update actions.decoder msg actions.input)
                |> Result.mapError (Decode.CustomError Nothing)
                |> Result.withDefault ( actions.input, actions.result )
    in
    { actions | result = result, input = input }
