module Chapters.Customization exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html
import Support.CustomElementsDemo as Demo
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
            , Task.perform (Actions.logActionWithString "Demo")
                (Task.succeed "Interact to see results")
            )


chapter : Chapter { x | customization : Model }
chapter =
    Chapter.chapter "View Customization and Custom HTML Components"
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

Each field is rendered with a few classes that can be used to apply styles, also
`Field.class` and `Field.classList` can be used to add classes to field wrappers
and groups.




```elm
Field.text
    [ Field.label "Text Field"
    , Field.placeholder "Enter any text"
    , Field.hint "This field accepts any text input"
    , Field.required True
    ]
```

Will produce this markup:

```
<div class="field required text-field">
  <label for="text" id="text-label">Text Field</label>
  <div class="input-wrapper">
    <input type="text"
           autocomplete="off"
           placeholder="Enter any text"
           id="text"
           required=""
           class=""
           aria-describedby="text-hint">
  </div>
  <div class="hint" id="text-hint">This field accepts any text input</div>
</div>
```


## View Customizations and Custom HTML Elements.

`FormToolkit.View` provides functions for customizing the rendered HTML. The
default field rendering can be replaced with `customizeFields` for complete
control over the HTML structure. Error messages can be overridden with
`customizeErrors`, field groups with `customizeGroups`, repeatable containers
with `customizeRepeatableFields`, and individual repeating instances with
`customizeRepeatingFieldTemplates`.

This demo uses custom HTML elements for a map-based address selector with
Nominatim and Leaflet, and a tag selector using Choices.js.

<component with-label="Demo"/>

The `customizeFields` function receives pre-rendered elements `labelHtml`,
`hintHtml`, event handlers `inputOnChange`, and field `attributes` for each field.
Return `Just` with custom HTML to override the default rendering, or `Nothing` to
use the default.

```elm
field
    |> View.fromField FormChanged
    |> View.customizeFields
        (\\{ attributes, labelHtml, hintHtml, inputOnChange } ->
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
                                            (\\jsonValue ->
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
                                , Events.on "choicesChange"
                                    (Decode.at [ "detail", "value" ] (Decode.list Decode.string)
                                        |> Decode.map
                                            (\\selectedValues ->
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

                _ ->
                    Nothing
        )
    |> View.toHtml
```

A full example can be found [here](https://github.com/maca/elm-form-toolkit/blob/main/docs/src/Support/CustomElementsDemo.elm).

"""
