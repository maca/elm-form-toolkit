module Chapters.Internationalization exposing (Model, chapter, init)

import ElmBook.Chapter as Chapter exposing (Chapter)


type alias Book book =
    { book | internationalization : Model }


type alias Model =
    {}


init : Model
init =
    {}


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
