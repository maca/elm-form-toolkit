module Chapters.Customization exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html
import Support.NominatimDemo as Demo
import Task


type alias Model =
    { demo : Demo.Model }


type Msg
    = DemoUpdated Demo.Msg


init : Model
init =
    { demo = Demo.init }


update : Msg -> Model -> ( Model, Cmd (ElmBook.Msg state) )
update msg model =
    case msg of
        DemoUpdated demoMsg ->
            let
                demo =
                    Demo.update demoMsg model.demo
            in
            ( { model | demo = demo }
            , Task.perform (Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString demo.result))
            )


chapter : Chapter { x | customization : Model }
chapter =
    Chapter.chapter "Customization"
        |> Chapter.withStatefulComponentList
            [ ( "Demo"
              , \book ->
                    book.customization.demo
                        |> Demo.view
                        |> Html.map
                            (Actions.updateStateWithCmdWith
                                (\msg state ->
                                    update (DemoUpdated msg) state.customization
                                        |> Tuple.mapFirst
                                            (\customization ->
                                                { state | customization = customization }
                                            )
                                )
                            )
              )
            ]
        |> Chapter.render markdownContent


markdownContent : String
markdownContent =
    """
# Customization

form-toolkit provides powerful customization capabilities that allow you to control exactly how your forms are rendered. This is particularly useful when you need to integrate custom UI components or create specialized field types.


## Custom Field Rendering with View.customizeFields

The `View.customizeFields` function gives you complete control over how each field is rendered. This is especially useful when you want to:

- Integrate custom HTML elements (like maps, date pickers, etc.)
- Apply custom styling or layouts
- Add interactive components that aren't standard form inputs
- Pattern match on field identifiers to render different fields differently


## Example: Address Selection with Map

This example demonstrates how to create a custom address form that uses OpenStreetMap and Nominatim reverse geocoding. Users can click on a map to automatically populate address fields.


<component with-label="Demo"/>


### Key Concepts

**1. Define a Regular Field**

We use a regular `Field.text` with an identifier for our map component:

```elm
Field.text
    [ Field.label "Select Location on Map"
    , Field.identifier AddressMap
    , Field.name "address-map"
    , Field.hint "Click on the map to select a location"
    ]
```

**2. Custom Rendering with Pattern Matching**

We use `View.customizeFields` to pattern match on the field identifier and render custom HTML:

```elm
viewCustomFields : Field AddressFields -> Html Msg
viewCustomFields field =
    field
        |> View.fromField FormChanged
        |> View.customizeFields
            (\\config ->
                case config.identifier of
                    Just AddressMap ->
                        -- Render custom map element
                        Html.div
                            [ Attr.class "field" ]
                            [ config.label [ Attr.class "input-label" ]
                            , Html.node "nominatim-reverse-geocoding"
                                [ Events.on "address-selected"
                                    (Decode.map MapAddressSelected Decode.value)
                                ]
                                []
                            , case config.hint of
                                Just hintText ->
                                    Html.p [ Attr.class "hint" ] [ Html.text hintText ]
                                Nothing ->
                                    Html.text ""
                            ]

                    _ ->
                        -- Default rendering for other fields
                        Html.div
                            [ Attr.class "field"
                            , Attr.classList [ ( "required", config.isRequired ) ]
                            ]
                            [ config.label [ Attr.class "input-label" ]
                            , Html.div
                                [ Attr.class "input-wrapper" ]
                                [ config.fieldHtml []
                                ]
                            , case config.errors of
                                err :: _ ->
                                    Html.p [ Attr.class "errors" ] [ Html.text err ]
                                [] ->
                                    case config.hint of
                                        Just hintText ->
                                            Html.p [ Attr.class "hint" ] [ Html.text hintText ]
                                        Nothing ->
                                            Html.text ""
                            ]
            )
        |> View.toHtml
```

**3. Handling Custom Events**

When the user clicks the map, we receive a custom event with address data:

```elm
type Msg
    = FormChanged (Field.Msg AddressFields)
    | MapAddressSelected Decode.Value
    | FormSubmitted

update msg model =
    case msg of
        MapAddressSelected value ->
            case decodeAddressSelection value of
                Ok addressData ->
                    let
                        updatedFields =
                            model.addressFields
                                |> Field.updateWithId Street
                                    (Field.value (Value.string (Maybe.withDefault "" addressData.street)))
                                |> Field.updateWithId City
                                    (Field.value (Value.string (Maybe.withDefault "" addressData.city)))
                                -- ... update other fields
                    in
                    { model | addressFields = updatedFields }
```


### The customizeFields Configuration

The configuration record passed to `customizeFields` contains:

- **identifier**: The field's identifier (if any) - use this for pattern matching
- **isRequired**: Boolean indicating if the field is required
- **label**: Function that takes HTML attributes and returns the label element
- **fieldHtml**: Function that takes HTML attributes and returns the input element
- **errors**: List of error messages for this field
- **hint**: Optional hint text for the field


## Complete Example

You can find the complete working example here:
[NominatimDemo.elm](https://github.com/maca/elm-form-toolkit/blob/main/docs/src/Support/NominatimDemo.elm)

This example demonstrates:
- Custom HTML element integration (Web Components)
- Pattern matching on field identifiers for custom rendering
- Handling custom JavaScript events in Elm
- Programmatically updating field values with `Field.updateValue`
- Custom parsing logic for coordinates
"""
