module ViewTest exposing (suite)

import Expect
import FormToolkit.Decode as Decode exposing (Error(..))
import FormToolkit.Input as Input
import FormToolkit.View as View
import Html.Attributes exposing (name)
import Json.Decode exposing (Error(..))
import Json.Encode
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query exposing (find, findAll, index)
import Test.Html.Selector exposing (attribute, containing, tag, text)


suite : Test
suite =
    describe "Html"
        [ test "adding new repeatable inputs" <|
            \_ ->
                let
                    { result } =
                        initActions bandFields
                            |> fillInput "band-name" "Love and Rockets"
                            |> fillInput "member-name" "Daniel Ash"
                            |> fillInput "member-age" "67"
                            |> clickButton "Add"
                            |> fillInputWithIndex 1 "member-name" "David J"
                            |> fillInputWithIndex 1 "member-age" "67"
                            |> clickButton "Add"
                            |> fillInputWithIndex 2 "member-name" "Kevin Haskins"
                            |> fillInputWithIndex 2 "member-age" "64"
                in
                Expect.equal result
                    (Ok
                        { name = "Love and Rockets"
                        , members =
                            [ { name = "Daniel Ash", age = 67 }
                            , { name = "David J", age = 67 }
                            , { name = "Kevin Haskins", age = 64 }
                            ]
                        }
                    )
        ]


type alias Actions id val a =
    { input : Input.Input id val
    , decoder : Decode.Decoder id val a
    , result : Result (List (Decode.Error id val)) a
    }


fillInputWithIndex : Int -> String -> String -> Actions id val a -> Actions id val a
fillInputWithIndex idx inputName inputText =
    interact (findAll [ attribute (name inputName) ] >> index idx) (Event.input inputText)


clickButton : String -> Actions id val a -> Actions id val a
clickButton buttonText =
    interact (find [ tag "button", containing [ text buttonText ] ]) Event.click


fillInput : String -> String -> Actions id val a -> Actions id val a
fillInput inputName inputText =
    interact (findInput inputName) (Event.input inputText)


findInput : String -> Query.Single msg -> Query.Single msg
findInput inputName =
    find [ attribute (name inputName) ]


initActions : Input.Input BandFields val -> Actions BandFields val Band
initActions input =
    { input = input
    , decoder = bandDecoder
    , result = Err [ Decode.CustomError Nothing "Not modified" ]
    }


interact :
    (Query.Single (Input.Msg id val) -> Query.Single (Input.Msg id val))
    -> ( String, Json.Encode.Value )
    -> Actions id val a
    -> Actions id val a
interact matcher event actions =
    let
        query =
            actions.input
                |> View.fromInput identity
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


type BandFields
    = BandMembers
    | BandName
    | MemberName
    | MemberAge


bandFields : Input.Input BandFields val
bandFields =
    Input.group []
        [ Input.text
            [ Input.label "Band Name"
            , Input.required True
            , Input.identifier BandName
            , Input.name "band-name"
            ]
        , Input.group
            [ Input.label "Members (max 5)" ]
            [ Input.repeatable
                [ Input.identifier BandMembers
                , Input.repeatableMin 1
                , Input.repeatableMax 5
                , Input.name "band-members"
                ]
                (Input.group []
                    [ Input.text
                        [ Input.label "Member Name"
                        , Input.identifier MemberName
                        , Input.name "member-name"
                        ]
                    , Input.int
                        [ Input.label "Member Age"
                        , Input.identifier MemberAge
                        , Input.name "member-age"
                        ]
                    ]
                )
                []
            ]
        ]


type alias Band =
    { name : String
    , members : List Person
    }


type alias Person =
    { name : String
    , age : Int
    }


bandDecoder : Decode.Decoder BandFields val Band
bandDecoder =
    Decode.map2 Band
        (Decode.field BandName Decode.string)
        (Decode.field BandMembers (Decode.list personDecoder))


personDecoder : Decode.Decoder BandFields val Person
personDecoder =
    Decode.map2 Person
        (Decode.field MemberName Decode.string)
        (Decode.field MemberAge Decode.int)
