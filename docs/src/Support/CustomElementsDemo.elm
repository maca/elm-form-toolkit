module Support.CustomElementsDemo exposing (Model, Msg, init, update, view)

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
import Json.Encode as Encode



-- BROWSER.SANDBOX PROGRAM (for standalone usage)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- TYPES


type alias Model =
    { formFields : Field DemoFields
    , result : Maybe (Result (FormToolkit.Error.Error DemoFields) Demo)
    }


type alias Demo =
    { addressData : Maybe String
    , selectedTags : Maybe String
    , isEnabled : Bool
    }


type DemoFields
    = AddressMap
    | TagsSelector
    | ToggleSwitch


type Msg
    = FormChanged (Field.Msg DemoFields)



-- INIT


init : Model
init =
    { formFields = formFieldsDefinition
    , result = Nothing
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged fieldMsg ->
            let
                ( formFields, result ) =
                    Parse.parseUpdate demoParser fieldMsg model.formFields
            in
            { model
                | formFields = formFields
                , result = Just result
            }



-- FORM DEFINITION


formFieldsDefinition : Field DemoFields
formFieldsDefinition =
    Field.group
        [ Field.class "components-demo" ]
        [ Field.text
            [ Field.label "Select Location on Map"
            , Field.identifier AddressMap
            , Field.name "address-map"
            , Field.hint "Click on the map to select a location"
            ]
        , Field.text
            [ Field.label "Select Tags"
            , Field.identifier TagsSelector
            , Field.name "tags-selector"
            , Field.hint "Choose multiple tags using Choices.js"
            ]
        , Field.checkbox
            [ Field.label "Enable Feature"
            , Field.identifier ToggleSwitch
            , Field.name "toggle-switch"
            , Field.hint "Toggle to enable or disable"
            ]
        ]



-- PARSER


demoParser : Parse.Parser DemoFields Demo
demoParser =
    Parse.map3 Demo
        (Parse.field AddressMap
            (Parse.maybe (Parse.json |> Parse.map (Encode.encode 2)))
        )
        (Parse.field TagsSelector (Parse.maybe Parse.string))
        (Parse.field ToggleSwitch Parse.bool)



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h4 [] [ Html.text "Custom Elements Demo" ]
        , Html.form
            [ Attr.attribute "novalidate" "true"
            ]
            [ viewCustomFields model.formFields
            ]
        , Html.div
            [ Attr.class "milligram" ]
            [ case model.result of
                Just (Ok demo) ->
                    successMessage demo

                _ ->
                    Html.text ""
            ]
        ]


viewCustomFields : Field DemoFields -> Html Msg
viewCustomFields field =
    field
        |> View.fromField FormChanged
        |> View.customizeFields
            (\{ attributes, labelHtml, hintHtml, inputOnChange, inputOnCheck } ->
                case attributes.identifier of
                    Just AddressMap ->
                        Just
                            (Html.div
                                [ Attr.class "field" ]
                                [ labelHtml []
                                , Html.node "nominatim-reverse-geocoding"
                                    [ Events.on "address-selected"
                                        (Decode.at [ "detail" ] Decode.value
                                            |> Decode.map
                                                (\jsonValue ->
                                                    inputOnChange
                                                        (Value.json jsonValue)
                                                        { selectionStart = 0, selectionEnd = 0 }
                                                )
                                        )
                                    ]
                                    []
                                , hintHtml []
                                ]
                            )

                    Just TagsSelector ->
                        Just
                            (Html.div
                                [ Attr.class "field" ]
                                [ labelHtml []
                                , Html.node "choices-multi-select"
                                    [ Attr.attribute "placeholder" "Select tags..."
                                    , Attr.attribute "remove-button" "true"
                                    , Events.on "choicesChange"
                                        (Decode.at [ "detail", "value" ] (Decode.list Decode.string)
                                            |> Decode.map
                                                (\selectedValues ->
                                                    inputOnChange
                                                        (Value.string (String.join "," selectedValues))
                                                        { selectionStart = 0, selectionEnd = 0 }
                                                )
                                        )
                                    ]
                                    []
                                , hintHtml []
                                ]
                            )

                    Just ToggleSwitch ->
                        Just
                            (Html.div
                                [ Attr.class "field" ]
                                [ labelHtml []
                                , Html.label
                                    [ Attr.class "switch" ]
                                    [ Html.input
                                        [ Attr.type_ "checkbox"
                                        , Events.onCheck inputOnCheck
                                        ]
                                        []
                                    , Html.span [ Attr.class "slider" ] []
                                    ]
                                , hintHtml []
                                ]
                            )

                    _ ->
                        Nothing
            )
        |> View.toHtml


successMessage : Demo -> Html msg
successMessage demo =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#e8f5e8"
        , Attr.style "border-radius" "4px"
        ]
        [ Html.div [] [ Html.text "Form parsed successfully!" ]
        , Html.div [ Attr.style "padding-bottom" "2rem" ]
            [ Html.h5 [] [ Html.text "Address Data:" ]
            , case demo.addressData of
                Just data ->
                    Html.pre [ Attr.style "white-space" "pre-wrap" ] [ Html.text data ]

                Nothing ->
                    Html.div [ Attr.style "color" "#999" ] [ Html.text "No address selected" ]
            ]
        , Html.div [ Attr.style "padding-bottom" "2rem" ]
            [ Html.h5 [] [ Html.text "Selected Tags:" ]
            , case demo.selectedTags of
                Just tags ->
                    Html.div []
                        (tags
                            |> String.split ","
                            |> List.filter (\s -> not (String.isEmpty s))
                            |> List.map
                                (\tag ->
                                    Html.span
                                        [ Attr.style "display" "inline-block"
                                        , Attr.style "background" "#9b4dca"
                                        , Attr.style "color" "white"
                                        , Attr.style "padding" "0.3rem 0.6rem"
                                        , Attr.style "margin" "0.2rem"
                                        , Attr.style "border-radius" "3px"
                                        ]
                                        [ Html.text tag ]
                                )
                        )

                Nothing ->
                    Html.div [ Attr.style "color" "#999" ] [ Html.text "No tags selected" ]
            ]
        , Html.div [ Attr.style "padding-bottom" "2rem" ]
            [ Html.h5 [] [ Html.text "Feature Status:" ]
            , Html.div []
                [ Html.span
                    [ Attr.style "display" "inline-block"
                    , Attr.style "background"
                        (if demo.isEnabled then
                            "#32b643"

                         else
                            "#999"
                        )
                    , Attr.style "color" "white"
                    , Attr.style "padding" "0.3rem 0.6rem"
                    , Attr.style "border-radius" "3px"
                    ]
                    [ Html.text
                        (if demo.isEnabled then
                            "Enabled"

                         else
                            "Disabled"
                        )
                    ]
                ]
            ]
        ]
