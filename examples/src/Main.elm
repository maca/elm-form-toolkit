module Main exposing (main)

import Browser
import FormToolkit.Decode as Decode
import FormToolkit.Input as Input exposing (Input)
import FormToolkit.View as View
import Html exposing (Html)
import Html.Events exposing (onClick, onSubmit)
import Json.Encode
import Result


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { form : Input TeamFields
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
    = FormChanged (Input.Msg TeamFields)
    | FormSubmitted


teamFields : Input.Input TeamFields
teamFields =
    Input.group []
        [ Input.text
            [ Input.label "Team Name!"
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
        ]



-- <div class="mb-4">
--     <label class="block text-gray-700 text-sm font-bold mb-2" for="username">
--     Username
--     </label>
--     <input class="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" id="username" type="text" placeholder="Username">
-- </div>


init : Model
init =
    { -- You want to keep the stateful input group in your model, don't worry there are
      -- no functions or weird stuff in there
      form = teamFields
    , submitted = False
    , team = Nothing
    , json = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( form, result ) =
                    -- Validates and produces result with decoder and updates with Msg
                    Input.update teamDecoder inputMsg model.form
            in
            { model | form = form, team = Result.toMaybe result }

        FormSubmitted ->
            { model
                | submitted = True

                -- Uses Input.name values as keys to build a json object
                , json =
                    Decode.decode Decode.json model.form
                        |> Result.toMaybe
            }



-- VIEW


view : Model -> Html Msg
view model =
    Html.form
        [ onSubmit FormSubmitted ]
        [ -- Render the form
          Input.toView FormChanged model.form
            |> View.customizeInput
                (\params ->
                    Html.div
                        []
                        [--  params.label
                         -- , params.input
                        ]
                )
            -- { isRequired : Bool
            --  , labelHtml : Html msg
            --  , inputHtml : Html msg
            --  , errorsHtml : List (Html msg)
            --  , hint : Maybe String
            --  }
            |> View.toHtml
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


teamDecoder : Decode.Decoder TeamFields Team
teamDecoder =
    Decode.map2 Team
        (Decode.field TeamName Decode.string)
        (Decode.field TeamMembers (Decode.list personDecoder))


personDecoder : Decode.Decoder TeamFields Person
personDecoder =
    Decode.map2 Person
        (Decode.field MemberName Decode.string)
        (Decode.field MemberAge Decode.int)
