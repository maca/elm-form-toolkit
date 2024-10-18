module Main exposing (main)

import Browser
import FormToolkit.Decode as Decode
import FormToolkit.Input as Input exposing (Input)
import Html exposing (Html)
import Html.Events exposing (onClick, onSubmit)
import Json.Encode
import Result


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { formFields : Input TeamFields Never
    , submitted : Bool
    , team : Maybe Team
    , json : Maybe Json.Encode.Value
    }


type TeamFields
    = TeamMembers
    | TeamName
    | MemberName
    | MemberAge


type Msg
    = FormChanged (Input.Msg TeamFields Never)
    | FormSubmitted


teamFields : Input.Input TeamFields val
teamFields =
    -- Input.group []
    -- [
    Input.text
        [ Input.label "Team Name"
        , Input.required True
        , Input.identifier TeamName
        , Input.name "team-name"
        ]



-- , Input.group
--     [ Input.label "Members (max 5)" ]
--     [ Input.repeatable
--         [ Input.identifier TeamMembers
--         , Input.repeatableMin 1
--         , Input.repeatableMax 5
--         , Input.name "team-members"
--         ]
--         (Input.group []
--             [ Input.text
--                 [ Input.label "Member Name"
--                 , Input.required True
--                 , Input.identifier MemberName
--                 , Input.name "member-name"
--                 ]
--             , Input.int
--                 [ Input.label "Member Age"
--                 , Input.identifier MemberAge
--                 , Input.name "member-age"
--                 ]
--             ]
--         )
--         []
--     ]
-- ]


init : Model
init =
    { -- You want to keep the stateful input group in your model, don't worry there are
      -- no functions or weird stuff in there
      formFields = teamFields
    , submitted = False
    , team = Nothing
    , json = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( formFields, result ) =
                    -- Validates and produces result with decoder and updates with Msg
                    Input.update (Decode.format removeVowels) inputMsg model.formFields
            in
            { model | formFields = formFields, team = Nothing }

        FormSubmitted ->
            { model
                | submitted = True

                -- Uses Input.name values as keys to build a json object
                , json =
                    Decode.decode Decode.json model.formFields
                        |> Result.toMaybe
            }



-- VIEW


view : Model -> Html Msg
view model =
    Html.form
        [ onSubmit FormSubmitted ]
        [ -- Render the form
          Input.toHtml FormChanged model.formFields
        , Html.button [ onClick FormSubmitted ] [ Html.text "Submit" ]
        ]


type alias Team =
    { name : String
    , members : List Person
    }


type alias Person =
    { name : String
    , age : Int
    }


teamDecoder : Decode.Decoder TeamFields val Team
teamDecoder =
    Decode.map2 Team
        (Decode.field TeamName (Decode.format (String.replace "a" "")))
        (Decode.field TeamMembers (Decode.succeed []))



-- personDecoder : Decode.Decoder TeamFields val Person
-- personDecoder =
--     Decode.map2 Person
--         (Decode.field MemberName (Decode.format (String.replace "a" "")))
--         (Decode.field MemberAge Decode.int)


removeVowels : String -> String
removeVowels =
    String.replace "a" ""
        >> String.replace "e" ""
        >> String.replace "i" ""
        >> String.replace "o" ""
        >> String.replace "u" ""
