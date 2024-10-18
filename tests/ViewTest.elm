module ViewTest exposing (suite)

import Expect
import FormToolkit.Decode as Decode exposing (Error(..))
import FormToolkit.Input as Input
import Json.Decode exposing (Error(..))
import Support.Interaction as Interaction exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Html"
        [ test "adding new repeatable inputs" <|
            \_ ->
                let
                    { result } =
                        Interaction.init bandDecoder bandFields
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
