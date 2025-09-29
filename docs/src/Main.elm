module Main exposing (main)

import Chapters.Customization as Customization
import Chapters.FormattingAndValidation as FormattingAndValidation
import Chapters.GettingStarted as GettingStarted
import Chapters.Overview as Overview
import ElmBook exposing (Book, book)
import ElmBook.Chapter as Chapter
import ElmBook.StatefulOptions as StatefulOptions


type alias Model =
    { gettingStarted : GettingStarted.Model
    , overview : Overview.Model
    , formattingAndValidation : FormattingAndValidation.Model
    , customization : Customization.Model
    }


init : Model
init =
    { gettingStarted = GettingStarted.init
    , overview = Overview.init
    , formattingAndValidation = FormattingAndValidation.init
    , customization = Customization.init
    }


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
              , [ GettingStarted.chapter
                , Overview.chapter
                , FormattingAndValidation.chapter
                , Customization.chapter
                ]
              )
            ]
