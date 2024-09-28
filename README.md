# Form Toolkit

A package for building, validating, and rendering forms.


Intends to
- Be opinionated but flexible. 
- Allow for [declarative building](FormToolkit.Input) of complex forms, with
  [repeating fields](FormToolkit.Input#repeatable), using an API similar to elm `Html`.
- Provide [default rendering](FormToolkit.Input#toHtml) of input groups with label
  and errors, but allow for [customization](FormToolkit.View).
- Permit custom validations, and provide standard ones such as 
  [presence](FormToolkit.Input#required), and [minimum](FormToolkit.Input#min) 
  and [maximum](FormToolkit.Input#max) scalar values.
- Use elm `Json.Decode` style approach for validating and mapping form inputs to Elm types. 
  With the expected functions for decoding [int](FormToolkit.Decode#int),
  [string](FormToolkit.Decode#string), [float](FormToolkit.Decode#float),
  [bool](FormToolkit.Decode#bool), [posix](FormToolkit.Decode#posix),
  and more. For mapping to and constructing types providing
  [map](FormToolkit.Decode#map), [mapN](FormToolkit.Decode#map2), and
  [andThen](FormToolkit.Decode#andThen), and additionally
  [andMap](FormToolkit.Decode#andMap) to build decoding pipelines.  
- Allow building of dynamic forms from a JSON specification and [conversion of
  form inputs to a JSON value](FormToolkit.Decode#json) to be forwarded as is
  to a backend.



```elm
import Browser
import FormToolkit.Decode as Decode
import FormToolkit.Input as Input exposing (Input)
import Html exposing (Html)
import Html.Events exposing (onClick, onSubmit)
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
          Input.toHtml FormChanged model.form
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
```
