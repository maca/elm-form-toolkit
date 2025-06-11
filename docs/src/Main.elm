module Main exposing (main)

import Chapters.Overview as Overview
import ElmBook exposing (Book, book)
import ElmBook.Chapter as Chapter
import ElmBook.StatefulOptions as StatefulOptions


type alias Model =
    { overview : Overview.Model }


init : Model
init =
    { overview = Overview.init }


main : Book Model
main =
    book "Elm-UI"
        |> ElmBook.withStatefulOptions
            [ StatefulOptions.initialState init
            ]
        |> ElmBook.withChapterGroups
            [ ( ""
              , [ Overview.chapter
                , Chapter.chapterLink
                    { title = "Api"
                    , url = "https://example.com"
                    }
                ]
              )
            , ( "Usage"
              , [ Overview.chapter
                ]
              )
            ]
