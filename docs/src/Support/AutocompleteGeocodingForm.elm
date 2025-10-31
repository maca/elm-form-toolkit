module Support.AutocompleteGeocodingForm exposing (Model, Msg, init, main, update, view)

import Browser
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes as Attr
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url



-- BROWSER.ELEMENT PROGRAM


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- TYPES


type alias Model =
    { addressField : Field Never
    , result : Maybe String
    }


type alias SearchResult =
    { displayName : String
    , json : Encode.Value
    }


type Msg
    = FormChanged (Field.Msg Never)
    | GotSearchResults String (Result Http.Error (List SearchResult))



-- INIT


init : Model
init =
    { addressField = addressField
    , result = Nothing
    }


addressField : Field Never
addressField =
    Field.strictAutocomplete
        [ Field.label "Search Address"
        , Field.name "address"
        , Field.placeholder "Type an address..."
        , Field.required True
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormChanged fieldMsg ->
            let
                ( updatedField, result ) =
                    Parse.parseUpdate Parse.string fieldMsg model.addressField

                query =
                    Parse.parse Parse.stringLenient model.addressField
            in
            case ( result, query ) of
                ( Err _, Ok q ) ->
                    ( { model
                        | addressField = updatedField
                        , result = Nothing
                      }
                    , Cmd.batch
                        [ searchGeocode q
                        , Http.cancel "geocode-autocomplete"
                        ]
                    )

                _ ->
                    ( { model
                        | addressField = updatedField
                        , result = Result.toMaybe result
                      }
                    , Cmd.none
                    )

        GotSearchResults query results ->
            let
                options =
                    results
                        |> Result.withDefault []
                        |> List.map
                            (\opt ->
                                ( if
                                    String.contains (String.toLower query)
                                        (String.toLower opt.displayName)
                                  then
                                    opt.displayName

                                  else
                                    opt.displayName ++ " (" ++ query ++ ")"
                                , Value.string (Encode.encode 2 opt.json)
                                )
                            )

                updatedField =
                    model.addressField
                        |> Field.updateAttribute (Field.options options)
            in
            ( { model | addressField = updatedField }
            , Cmd.none
            )



-- HTTP


searchGeocode : String -> Cmd Msg
searchGeocode query =
    Http.request
        { method = "GET"
        , headers = []
        , url = "https://photon.komoot.io/api?q=" ++ Url.percentEncode query ++ "&limit=5"
        , body = Http.emptyBody
        , expect =
            Http.expectJson (GotSearchResults query)
                searchResultsDecoder
        , timeout = Nothing
        , tracker = Just "geocode-autocomplete"
        }


searchResultsDecoder : Decode.Decoder (List SearchResult)
searchResultsDecoder =
    Decode.field "features"
        (Decode.list
            (Decode.map2 SearchResult
                (Decode.field "properties" addressDecoder)
                Decode.value
            )
        )


addressDecoder : Decode.Decoder String
addressDecoder =
    Decode.map5
        (\name street housenumber postcode city ->
            [ name
            , Maybe.map2 (\h s -> h ++ " " ++ s) street housenumber
                |> Maybe.withDefault (street |> Maybe.withDefault "")
            , postcode
            , city
            ]
                |> List.filter (not << String.isEmpty)
                |> String.join ", "
        )
        (Decode.maybe (Decode.field "name" Decode.string) |> Decode.map (Maybe.withDefault ""))
        (Decode.maybe (Decode.field "street" Decode.string))
        (Decode.maybe (Decode.field "housenumber" Decode.string))
        (Decode.maybe (Decode.field "postcode" Decode.string) |> Decode.map (Maybe.withDefault ""))
        (Decode.maybe (Decode.field "city" Decode.string) |> Decode.map (Maybe.withDefault ""))



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "milligram" ]
        [ model.addressField |> Field.toHtml FormChanged
        , case model.result of
            Just resultString ->
                Html.div
                    [ Attr.style "margin-top" "1rem"
                    , Attr.style "padding" "1rem"
                    , Attr.style "background" "#e8f5e8"
                    , Attr.style "border-radius" "4px"
                    ]
                    [ Html.h5 [] [ Html.text "Selected Address:" ]
                    , Html.pre
                        [ Attr.style "white-space" "pre-wrap"
                        , Attr.style "word-wrap" "break-word"
                        ]
                        [ Html.text resultString ]
                    ]

            Nothing ->
                Html.text ""
        ]
