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
                case config.attributes.identifier of
                    Just AddressMap ->
                        -- Render custom map element
                        Just
                            (Html.div
                                [ Attr.class "field" ]
                                [ config.labelHtml []
                                , Html.node "nominatim-reverse-geocoding"
                                    [ Events.on "address-selected"
                                        (Decode.map MapAddressSelected Decode.value)
                                    ]
                                    []
                                , config.hintHtml []
                                ]
                            )

                    _ ->
                        -- Return Nothing to use default rendering
                        Nothing
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

- **attributes**: Record containing the field's attributes including:
  - **identifier**: The field's identifier (if any) - use `config.attributes.identifier` for pattern matching
  - Other field attributes like name, placeholder, etc.
- **isRequired**: Boolean indicating if the field is required
- **labelHtml**: Pre-rendered label HTML element (takes View attributes list)
- **fieldHtml**: Pre-rendered input HTML element (takes View attributes list)
- **hintHtml**: Pre-rendered hint HTML element (takes View attributes list)
- **errors**: List of error messages for this field
- **class**: CSS classes applied to the field
- **events**: Record containing event handlers:
  - **inputOnChange**: Handler for input changes
  - **inputOnCheck**: Handler for checkbox changes
  - **inputOnBlur**: Handler for blur events
  - **inputOnFocus**: Handler for focus events

**Important**: The function should return `Just (Html msg)` for custom rendering, or `Nothing` to use the default rendering.


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
