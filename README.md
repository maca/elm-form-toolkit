# Form Toolkit

A package for building, parsing, validating, and rendering forms.

See interactive documentation
[here](https://form-toolkit.bitmunge.com/usage/getting-started).

Goals:

- Be opinionated but flexible.
- Allow for declarative building of complex forms with repeating fields, using
  an API similar to Elm's `Html`.
- Provide default rendering of input groups with labels and errors using native
  inputs, but allow for rich customizations.
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
import FormToolkit.Parse as Parse
import FormToolkit.Field as Field exposing (Field)
import Html exposing (Html)
import Html.Events exposing (onClick, onSubmit)
import Json.Encode
import Result


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { formFields : Field TeamFields
    , submitted : Bool
    , team : Maybe Team
    }


type TeamFields
    = TeamMembers
    | TeamName
    | MemberName
    | MemberAge


type Msg
    = FormChanged (Field.Msg TeamFields)
    | FormSubmitted


teamFields : Field TeamFields
teamFields =
    Field.group []
        [ Field.text
            [ Field.label "Team Name"
            , Field.required True
            , Field.identifier TeamName
            , Field.name "team-name"
            ]
        , Field.group
            [ Field.label "Members (max 5)" ]
            [ Field.repeatable
                [ Field.identifier TeamMembers
                , Field.repeatableMin 1
                , Field.repeatableMax 5
                , Field.name "team-members"
                ]
                (Field.group []
                    [ Field.text
                        [ Field.label "Member Name"
                        , Field.required True
                        , Field.identifier MemberName
                        , Field.name "member-name"
                        ]
                    , Field.int
                        [ Field.label "Member Age"
                        , Field.identifier MemberAge
                        , Field.name "member-age"
                        ]
                    ]
                )
                []
            ]
        ]


init : Model
init =
    { -- You want to keep the stateful input group in your model, don't worry, there are
      -- no functions or weird stuff in there
      formFields = teamFields
    , submitted = False
    , team = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( formFields, result ) =
                    -- Validates and produces result with parser and updates with Msg
                    Parse.parseUpdate teamParser inputMsg model.formFields
            in
            { model
                | formFields = formFields
                , team = Result.toMaybe result
            }

        FormSubmitted ->
            { model
                -- Touch makes field errors visible
                | formFields = Field.touch model.formFields
                , submitted = True
            }



-- VIEW


view : Model -> Html Msg
view model =
    Html.form
        [ onSubmit FormSubmitted ]
        [ -- Render the form
          Field.toHtml FormChanged model.formFields
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


teamParser : Parse.Parser TeamFields Team
teamParser =
    Parse.map2 Team
        (Parse.field TeamName Parse.string)
        (Parse.field TeamMembers (Parse.list personParser))


personParser : Parse.Parser TeamFields Person
personParser =
    Parse.map2 Person
        (Parse.field MemberName Parse.string)
        (Parse.field MemberAge Parse.int)
```
