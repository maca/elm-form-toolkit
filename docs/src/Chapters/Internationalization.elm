module Chapters.Internationalization exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html
import Support.I18nForm as Demo
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


chapter : Chapter { x | internationalization : Model }
chapter =
    Chapter.chapter "Internationalization"
        |> Chapter.withStatefulComponentList
            [ ( "Demo"
              , \book ->
                    book.internationalization.demo
                        |> Demo.view
                        |> Html.map
                            (Actions.updateStateWithCmdWith
                                (\msg state ->
                                    update (DemoUpdated msg) state.internationalization
                                        |> Tuple.mapFirst
                                            (\internationalization ->
                                                { state | internationalization = internationalization }
                                            )
                                )
                            )
              )
            ]
        |> Chapter.render
            internationalizationMarkdown


internationalizationMarkdown : String
internationalizationMarkdown =
    """
Translation is achieved using `FormToolkit.View` functions to customize error messages and field labels.


## Translating Labels and Placeholders

Field labels, placeholders, and hints are translated directly in the field definition using `Field.label`, `Field.placeholder`, and `Field.hint`.

```elm
Field.text
    [ Field.label "Nombre"
    , Field.placeholder "Ingrese su nombre"
    , Field.hint "Este campo acepta cualquier texto"
    , Field.required True
    ]
```

## Translating Error Messages

Use `View.customizeErrors` to translate validation error messages. The function receives `FieldAttributes` which includes all field errors in the `errors` field.

```elm
model.shipmentFields
    |> View.fromField FormChanged
    |> View.customizeErrors errorToSpanish
    |> View.toHtml

errorToSpanish : View.FieldAttributes id -> String
errorToSpanish attributes =
    let
        errorToString error =
            case error of
                Error.IsBlank _ ->
                    "Este campo es obligatorio"

                Error.NotAnInteger _ ->
                    "Debe ser un número entero"

                Error.ValueTooSmall _ data ->
                    "El valor mínimo es " ++ toString data.min

                Error.CustomError _ message ->
                    message

                _ ->
                    "Error en el campo"
    in
    attributes.errors
        |> List.map errorToString
        |> String.join ", "
```

## Translating Repeatable Field Buttons

Use `Field.copies` to translate add and remove button text for repeatable fields.

```elm
Field.repeatable
    [ Field.label "Contactos"
    , Field.copies
        { addFieldsButton = "Añadir Contacto"
        , removeFieldsButton = "Eliminar"
        }
    ]
    (Field.group [] [ ... ])
    []
```

<component with-label="Demo"/>

A full example can be found [here](https://github.com/maca/elm-form-toolkit/blob/main/docs/src/Support/I18nForm.elm).
"""
