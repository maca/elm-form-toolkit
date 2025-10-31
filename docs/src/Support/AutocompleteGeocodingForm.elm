module Support.AutocompleteGeocodingForm exposing (Model, Msg, init, main, update, view)

import Browser
import FormToolkit.Error as Error exposing (Error)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes as Attr exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)
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
    , coordinates : Maybe Coords
    , result : Maybe (Result (Error Never) Encode.Value)
    }


type alias Coords =
    { lat : Float
    , lon : Float
    }


type alias SearchResult =
    { displayName : String
    , coords : Coords
    , json : Encode.Value
    }


type Msg
    = FormChanged (Field.Msg Never)
    | GotSearchResults (Result Http.Error (List SearchResult))
    | FormSubmitted



-- INIT


init : Model
init =
    { addressField = addressField
    , coordinates = Nothing
    , result = Nothing
    }


addressField : Field Never
addressField =
    Field.text
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
            in
            ( { model | addressField = updatedField }
            , result
                |> Result.map searchGeocode
                |> Result.withDefault Cmd.none
            )

        GotSearchResults (Ok results) ->
            let
                _ =
                    Debug.log "results" results

                options =
                    results |> List.map .displayName

                updatedField =
                    model.addressField
                        |> Field.updateAttribute (Field.stringOptions options)
            in
            ( { model | addressField = updatedField }
            , Cmd.none
            )

        GotSearchResults (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
            ( model, Cmd.none )

        FormSubmitted ->
            case Parse.parseValidate Parse.json model.addressField of
                ( updatedField, Ok jsonValue ) ->
                    let
                        coords =
                            extractCoordinates jsonValue
                    in
                    ( { model
                        | addressField = updatedField
                        , coordinates = coords
                        , result = Just (Ok jsonValue)
                      }
                    , Cmd.none
                    )

                ( updatedField, Err error ) ->
                    ( { model
                        | addressField = updatedField
                        , result = Just (Err error)
                      }
                    , Cmd.none
                    )



-- HTTP


searchGeocode : String -> Cmd Msg
searchGeocode query =
    Http.get
        { url =
            "https://photon.komoot.io/api?q="
                ++ Url.percentEncode query
                ++ "&limit=5"
        , expect = Http.expectJson GotSearchResults searchResultsDecoder
        }


searchResultsDecoder : Decode.Decoder (List SearchResult)
searchResultsDecoder =
    Decode.field "features"
        (Decode.list
            (Decode.map3 SearchResult
                (Decode.field "properties" addressDecoder)
                (Decode.at [ "geometry", "coordinates" ]
                    (Decode.map2 (\lon lat -> Coords lat lon)
                        (Decode.index 0 Decode.float)
                        (Decode.index 1 Decode.float)
                    )
                )
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


stringToFloat : String -> Decode.Decoder Float
stringToFloat str =
    case String.toFloat str of
        Just f ->
            Decode.succeed f

        Nothing ->
            Decode.fail ("Could not parse float: " ++ str)


extractCoordinates : Encode.Value -> Maybe Coords
extractCoordinates json =
    case Decode.decodeValue coordinatesDecoder json of
        Ok coords ->
            Just coords

        Err _ ->
            Nothing


coordinatesDecoder : Decode.Decoder Coords
coordinatesDecoder =
    Decode.map2 Coords
        (Decode.field "lat" Decode.float
            |> Decode.andThen
                (\lat ->
                    if lat == 0 then
                        Decode.oneOf
                            [ Decode.field "lat" Decode.string |> Decode.andThen stringToFloat
                            , Decode.succeed lat
                            ]

                    else
                        Decode.succeed lat
                )
        )
        (Decode.field "lon" Decode.float
            |> Decode.andThen
                (\lon ->
                    if lon == 0 then
                        Decode.oneOf
                            [ Decode.field "lon" Decode.string |> Decode.andThen stringToFloat
                            , Decode.succeed lon
                            ]

                    else
                        Decode.succeed lon
                )
        )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "milligram"
        , Attr.style "margin-top" "20px"
        , Attr.style "padding" "20px"
        , Attr.style "border" "1px solid #d1d1d1"
        , Attr.style "border-radius" "4px"
        ]
        [ Html.form
            [ onSubmit FormSubmitted, novalidate True ]
            [ model.addressField
                |> Field.toHtml FormChanged
            , Html.button
                [ onClick FormSubmitted
                , Attr.style "margin-top" "1rem"
                ]
                [ Html.text "Submit" ]
            ]
        , case model.result of
            Just (Ok jsonValue) ->
                Html.div []
                    [ case model.coordinates of
                        Just coords ->
                            viewMap coords

                        Nothing ->
                            Html.text ""
                    , viewJsonResult jsonValue
                    ]

            Just (Err _) ->
                Html.text ""

            Nothing ->
                Html.text ""
        ]


viewMap : Coords -> Html msg
viewMap coords =
    Html.node "nominatim-reverse-geocoding"
        [ Attr.attribute "lat" (String.fromFloat coords.lat)
        , Attr.attribute "lng" (String.fromFloat coords.lon)
        , Attr.style "margin-top" "1rem"
        ]
        []


viewJsonResult : Encode.Value -> Html msg
viewJsonResult json =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#f4f5f6"
        , Attr.style "border-radius" "4px"
        ]
        [ Html.h4 [] [ Html.text "Address JSON" ]
        , Html.pre
            [ Attr.style "white-space" "pre-wrap"
            , Attr.style "word-wrap" "break-word"
            ]
            [ Html.text (Encode.encode 2 json) ]
        ]
