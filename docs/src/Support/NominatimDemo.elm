module Support.NominatimDemo exposing (Model, Msg, init, update, view)

import Browser
import FormToolkit.Error
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import FormToolkit.View as View
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode



-- BROWSER.SANDBOX PROGRAM (for standalone usage)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- TYPES


type alias Model =
    { addressFields : Field AddressFields
    , submitted : Bool
    , result : Maybe (Result (FormToolkit.Error.Error AddressFields) Address)
    }


type alias Address =
    { street : String
    , city : String
    , state : String
    , country : String
    , postalCode : String
    , lat : Float
    , lng : Float
    }


type AddressFields
    = AddressMap
    | Street
    | City
    | State
    | Country
    | PostalCode
    | LatLng


type Msg
    = FormChanged (Field.Msg AddressFields)
    | MapAddressSelected Decode.Value
    | FormSubmitted



-- INIT


init : Model
init =
    { addressFields = addressFieldsDefinition
    , submitted = False
    , result = Nothing
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged fieldMsg ->
            let
                ( addressFields, result ) =
                    Parse.parseUpdate addressParser fieldMsg model.addressFields
            in
            { model
                | addressFields = addressFields
                , result = Just result
                , submitted = False
            }

        MapAddressSelected value ->
            case decodeAddressSelection value of
                Ok addressData ->
                    let
                        updatedFields =
                            model.addressFields
                                |> Field.updateWithId Street (Field.value (Value.string (Maybe.withDefault "" addressData.street)))
                                |> Field.updateWithId City (Field.value (Value.string (Maybe.withDefault "" addressData.city)))
                                |> Field.updateWithId State (Field.value (Value.string (Maybe.withDefault "" addressData.state)))
                                |> Field.updateWithId Country (Field.value (Value.string (Maybe.withDefault "" addressData.country)))
                                |> Field.updateWithId PostalCode (Field.value (Value.string (Maybe.withDefault "" addressData.postalCode)))
                                |> Field.updateWithId LatLng (Field.value (Value.string (String.fromFloat addressData.lat ++ "," ++ String.fromFloat addressData.lng)))
                    in
                    { model
                        | addressFields = updatedFields
                        , result = Parse.parse addressParser updatedFields |> Just
                    }

                Err _ ->
                    model

        FormSubmitted ->
            { model | submitted = True }



-- FORM DEFINITION


addressFieldsDefinition : Field AddressFields
addressFieldsDefinition =
    Field.group
        []
        [ Field.text
            [ Field.label "Select Location on Map"
            , Field.identifier AddressMap
            , Field.name "address-map"
            , Field.hint "Click on the map to select a location"
            ]
        , Field.text
            [ Field.label "Street"
            , Field.identifier Street
            , Field.name "street"
            , Field.required True
            ]
        , Field.group
            [ Field.class "inline-fields" ]
            [ Field.text
                [ Field.label "City"
                , Field.identifier City
                , Field.name "city"
                , Field.required True
                ]
            , Field.text
                [ Field.label "State"
                , Field.identifier State
                , Field.name "state"
                , Field.required True
                ]
            ]
        , Field.group
            [ Field.class "inline-fields" ]
            [ Field.text
                [ Field.label "Country"
                , Field.identifier Country
                , Field.name "country"
                , Field.required True
                ]
            , Field.text
                [ Field.label "Postal Code"
                , Field.identifier PostalCode
                , Field.name "postal-code"
                ]
            ]
        , Field.text
            [ Field.label "Coordinates"
            , Field.identifier LatLng
            , Field.name "lat-lng"
            , Field.disabled True
            ]
        ]



-- PARSER


addressParser : Parse.Parser AddressFields Address
addressParser =
    Parse.map7 Address
        (Parse.field Street Parse.string)
        (Parse.field City Parse.string)
        (Parse.field State Parse.string)
        (Parse.field Country Parse.string)
        (Parse.field PostalCode Parse.string)
        parseLat
        parseLng


parseLat : Parse.Parser AddressFields Float
parseLat =
    Parse.field LatLng Parse.string
        |> Parse.andThen
            (\latLngStr ->
                case String.split "," latLngStr of
                    latStr :: _ ->
                        case String.toFloat (String.trim latStr) of
                            Just lat ->
                                Parse.succeed lat

                            Nothing ->
                                Parse.fail "Invalid latitude"

                    _ ->
                        Parse.fail "Invalid coordinates format"
            )


parseLng : Parse.Parser AddressFields Float
parseLng =
    Parse.field LatLng Parse.string
        |> Parse.andThen
            (\latLngStr ->
                case String.split "," latLngStr of
                    _ :: lngStr :: _ ->
                        case String.toFloat (String.trim lngStr) of
                            Just lng ->
                                Parse.succeed lng

                            Nothing ->
                                Parse.fail "Invalid longitude"

                    _ ->
                        Parse.fail "Invalid coordinates format"
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
        [ Html.h4 [] [ Html.text "Address Selection with Map" ]
        , Html.form
            [ Events.onSubmit FormSubmitted
            , Attr.attribute "novalidate" "true"
            ]
            [ viewCustomFields model.addressFields
            , Html.button
                [ Events.onClick FormSubmitted
                , Attr.style "margin-top" "1rem"
                ]
                [ Html.text "Submit" ]
            ]
        , if model.submitted then
            case model.result of
                Just (Ok address) ->
                    successMessage address

                Just (Err _) ->
                    errorMessage

                Nothing ->
                    Html.text ""

          else
            Html.text ""
        ]


viewCustomFields : Field AddressFields -> Html Msg
viewCustomFields field =
    field
        |> View.fromField FormChanged
        |> View.customizeFields
            (\{ attributes, labelHtml, hintHtml } ->
                case attributes.identifier of
                    Just AddressMap ->
                        Just
                            (Html.div
                                [ Attr.class "field" ]
                                [ labelHtml []
                                , Html.node "nominatim-reverse-geocoding"
                                    [ Events.on "address-selected" (Decode.map MapAddressSelected Decode.value)
                                    ]
                                    []
                                , hintHtml []
                                ]
                            )

                    _ ->
                        Nothing
            )
        |> View.toHtml


successMessage : Address -> Html msg
successMessage address =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#e8f5e8"
        , Attr.style "border-radius" "4px"
        ]
        [ Html.div [] [ Html.text "Address selected successfully!" ]
        , Html.div [] [ Html.text ("Street: " ++ address.street) ]
        , Html.div [] [ Html.text ("City: " ++ address.city ++ ", " ++ address.state) ]
        , Html.div [] [ Html.text ("Country: " ++ address.country) ]
        , Html.div [] [ Html.text ("Coordinates: " ++ String.fromFloat address.lat ++ ", " ++ String.fromFloat address.lng) ]
        ]


errorMessage : Html msg
errorMessage =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#fde8e8"
        , Attr.style "border-radius" "4px"
        ]
        [ Html.text "Please fill in all required fields" ]



-- DECODERS


type alias AddressSelectionData =
    { lat : Float
    , lng : Float
    , street : Maybe String
    , city : Maybe String
    , state : Maybe String
    , country : Maybe String
    , postalCode : Maybe String
    }


decodeAddressSelection : Decode.Value -> Result Decode.Error AddressSelectionData
decodeAddressSelection value =
    Decode.decodeValue addressSelectionDecoder value


addressSelectionDecoder : Decode.Decoder AddressSelectionData
addressSelectionDecoder =
    Decode.at [ "detail" ]
        (Decode.map7 AddressSelectionData
            (Decode.field "lat" Decode.float)
            (Decode.field "lng" Decode.float)
            (Decode.at [ "address", "road" ] (Decode.maybe Decode.string))
            (Decode.at [ "address", "city" ]
                (Decode.maybe Decode.string)
                |> Decode.map
                    (\maybeCity ->
                        case maybeCity of
                            Just city ->
                                Just city

                            Nothing ->
                                Nothing
                    )
                |> Decode.andThen
                    (\maybeCity ->
                        case maybeCity of
                            Just city ->
                                Decode.succeed (Just city)

                            Nothing ->
                                Decode.at [ "address", "town" ] (Decode.maybe Decode.string)
                                    |> Decode.map
                                        (\maybeTown ->
                                            case maybeTown of
                                                Just town ->
                                                    Just town

                                                Nothing ->
                                                    Nothing
                                        )
                    )
            )
            (Decode.at [ "address", "state" ] (Decode.maybe Decode.string))
            (Decode.at [ "address", "country" ] (Decode.maybe Decode.string))
            (Decode.at [ "address", "postcode" ] (Decode.maybe Decode.string))
        )
