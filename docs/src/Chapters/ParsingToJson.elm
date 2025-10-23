module Chapters.ParsingToJson exposing (Model, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html
import Support.JsonPostForm as JsonPostForm
import Task


type alias Book book =
    { book | parsingToJson : Model }


type alias Model =
    { jsonPostForm : JsonPostForm.Model
    }


init : Model
init =
    { jsonPostForm = JsonPostForm.init
    }


update : JsonPostForm.Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update msg book =
    let
        ( newModel, cmd ) =
            let
                model =
                    book.parsingToJson

                ( updatedJsonPostForm, innerCmd ) =
                    JsonPostForm.update msg model.jsonPostForm
            in
            ( { model | jsonPostForm = updatedJsonPostForm }
            , Cmd.batch
                [ Cmd.map (Actions.updateStateWithCmdWith update) innerCmd
                , Task.perform (Actions.logActionWithString "Demo")
                    (Task.succeed "Press Submit to perform a request and see results")
                ]
            )
    in
    ( { book | parsingToJson = newModel }, cmd )


chapter : Chapter (Book book)
chapter =
    Chapter.chapter "Parsing to JSON - Demo"
        |> Chapter.withStatefulComponentList
            [ ( "JSON Post Form"
              , \book ->
                    book.parsingToJson.jsonPostForm
                        |> JsonPostForm.view
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            ]
        |> Chapter.render markdownContent


markdownContent : String
markdownContent =
    """

For forms whose input is not to be parsed to an Elm type, it's possible to parse
straight to a `Json.Encode.Value` with keys corresponding to the `Field.name`
attribute by using `Parse.json`. There's no need to set `Field.identifier`
attributes.

This value can then be sent straight to a server without any prior processing.

The type signature of the fields can be `Field String` when using `Field.name` attributes.

<component with-label="JSON Post Form"/>

## Parsing and Submitting JSON

When the **Submit** button is pressed, `Parse.parseValidate` returns the updated field
with validation errors and the parsed JSON value:

```elm
case Parse.parseValidate Parse.json model.shipmentFields of
    ( updatedField, Ok jsonValue ) ->
        ( { model
            | shipmentFields = updatedField
            , submitted = True
            , result = Nothing
          }
        , Http.post
            { url = "https://httpbin.org/anything"
            , body = Http.jsonBody jsonValue
            , expect = Http.expectJson GotResponse Decode.value
            }
        )

    ( updatedField, Err _ ) ->
        ( { model | shipmentFields = updatedField }, Cmd.none )
```

## Filling Fields from JSON

When the **Fill fields from JSON** button is pressed, `Field.updateValuesFromJson`
populates the fields from a JSON value:

```elm
case Field.updateValuesFromJson sampleValues model.shipmentFields of
    Ok fields ->
        fields

    Err _ ->
        model.shipmentFields
```

The JSON structure must match the field names:

```json
{
  "recipient": {
    "first-name": "José",
    "last-name": "García"
  },
  "address": {
    "street-name": "Avenida Revolución",
    "address-number": "456",
    "address-2": "Depto 3A",
    "postal-code": "03100",
    "state": "CDMX",
    "country": "156"
  },
  "credit-card": {
    "card-name": "José García",
    "card-number": "4532123456789012",
    "expire-month": "12/25",
    "cvc": "123"
  }
}
```

View the complete example [here](https://github.com/maca/elm-form-toolkit/blob/main/docs/src/Support/JsonPostForm.elm).

"""
