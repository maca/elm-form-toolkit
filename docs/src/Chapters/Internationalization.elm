module Chapters.Internationalization exposing (Model, Msg, chapter, init)

import ElmBook.Chapter as Chapter exposing (Chapter)


type alias Book book =
    { book | internationalization : Model }


type alias Model =
    {}


type Msg
    = NoOp


init : Model
init =
    {}


update : Msg -> Book book -> ( Book book, Cmd msg )
update msg book =
    case msg of
        NoOp ->
            ( book, Cmd.none )


chapter : Chapter (Book book)
chapter =
    Chapter.chapter "Internationalization"
        |> Chapter.withStatefulComponentList
            []
        |> Chapter.render
            internationalizationMarkdown


internationalizationMarkdown : String
internationalizationMarkdown =
    """
# Internationalization

This chapter will cover internationalization features and best practices for the Form Toolkit.

*Content coming soon...*
"""
