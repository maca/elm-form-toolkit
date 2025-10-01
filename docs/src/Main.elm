module Main exposing (main)

import Chapters.Customization as Customization
import Chapters.FieldReference as FieldReference
import Chapters.FormattingAndValidation as FormattingAndValidation
import Chapters.GettingStarted as GettingStarted
import Chapters.Overview as Overview
import ElmBook exposing (Book, book)
import ElmBook.Chapter as Chapter
import ElmBook.StatefulOptions as StatefulOptions


type alias Model =
    { gettingStarted : GettingStarted.Model
    , overview : Overview.Model
    , fieldReference : FieldReference.Model
    , formattingAndValidation : FormattingAndValidation.Model
    , customization : Customization.Model
    }


init : Model
init =
    { gettingStarted = GettingStarted.init
    , overview = Overview.init
    , fieldReference = FieldReference.init
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
            [ ( "Usage"
              , [ GettingStarted.chapter
                , Overview.chapter
                , FieldReference.chapter
                , FormattingAndValidation.chapter
                , Customization.chapter
                ]
              )
            , ( "Reference"
              , [ Chapter.chapterLink
                    { title = "Api"
                    , url = "https://example.com"
                    }
                ]
              )
            ]
