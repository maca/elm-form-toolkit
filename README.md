# Form Toolkit

A package for building, parsing and validating, and rendering forms.

Intends to

- Be opinionated but flexible.
- Allow for declarative building of complex forms with repeating fields, using
  an API similar to Elm's `Html`.
- Provide default rendering of input groups with labels and errors, but allow
  for customization.
- Permit custom validations and provide standard ones, such as presence and
  minimum and maximum scalar values.
- Enable validation and mapping of form inputs to custom types using an
  interface similar to Elm's `Json.Decode`, with the expected functions for
  decoding `string`, `int`, `float`, `bool`, and more, such as `posix`. For
  mapping to and constructing types, provide `map`, `mapN`, and `andThen`, with
  `andMap` additionally available to build decoding pipelines.
- Allow the building of dynamic forms from a JSON specification and conversion
  of form inputs to a JSON value to be forwarded as-is to a backend.

```elm
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
    Input.group []
        [ Input.text
            [ Input.label "Team Name"
            , Input.required True
            , Input.identifier TeamName
            , Input.name "team-name"
            ]
        , Input.group
            [ Input.label "Members (max 5)" ]
            [ Input.repeatable
                [ Input.identifier TeamMembers
                , Input.repeatableMin 1
                , Input.repeatableMax 5
                , Input.name "team-members"
                ]
                (Input.group []
                    [ Input.text
                        [ Input.label "Member Name"
                        , Input.required True
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
                    Input.update teamDecoder inputMsg model.formFields
            in
            { model | formFields = formFields, team = Result.toMaybe result }

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
        (Decode.field TeamName Decode.string)
        (Decode.field TeamMembers (Decode.list personDecoder))


personDecoder : Decode.Decoder TeamFields val Person
personDecoder =
    Decode.map2 Person
        (Decode.field MemberName Decode.string)
        (Decode.field MemberAge Decode.int)
```
