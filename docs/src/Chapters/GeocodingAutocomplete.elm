module Chapters.GeocodingAutocomplete exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html
import Support.AutocompleteGeocodingForm as Demo
import Task


type alias Book book =
    { book | geocodingAutocomplete : Model }


type alias Model =
    { demo : Demo.Model }


type Msg
    = DemoUpdated Demo.Msg


init : Model
init =
    { demo = Demo.init }


update : Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update msg book =
    case msg of
        DemoUpdated demoMsg ->
            let
                model =
                    book.geocodingAutocomplete

                ( demo, demoCmd ) =
                    Demo.update demoMsg model.demo
            in
            ( { book | geocodingAutocomplete = { model | demo = demo } }
            , Cmd.batch
                [ Cmd.map (DemoUpdated >> Actions.updateStateWithCmdWith update) demoCmd
                , Task.perform (Actions.logActionWithString "Demo")
                    (Task.succeed "Interact to see results")
                ]
            )


chapter : Chapter (Book book)
chapter =
    Chapter.chapter "Autocomplete Demo - Geocoding"
        |> Chapter.withStatefulComponentList
            [ ( "Demo"
              , \book ->
                    book.geocodingAutocomplete.demo
                        |> Demo.view
                        |> Html.map (DemoUpdated >> Actions.updateStateWithCmdWith update)
              )
            ]
        |> Chapter.render geocodingAutocompleteMarkdown


geocodingAutocompleteMarkdown : String
geocodingAutocompleteMarkdown =
    """
This demo shows how to use `Field.strictAutocomplete` with HTTP requests to create
a geocoding address search field.

<component with-label="Demo"/>

A full example can be found [here](https://github.com/maca/form-toolkit/blob/main/docs/src/Support/AutocompleteGeocodingForm.elm).
"""
