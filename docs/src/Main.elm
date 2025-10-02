module Main exposing (main)

import Chapters.Customization as Customization
import Chapters.Fields as Fields
import Chapters.FormattingAndValidation as FormattingAndValidation
import Chapters.GettingStarted as GettingStarted
import Chapters.Internationalization as Internationalization
import Chapters.Parsing as Parsing
import ElmBook exposing (Book, book)
import ElmBook.Chapter as Chapter
import ElmBook.StatefulOptions as StatefulOptions


type alias Model =
    { gettingStarted : GettingStarted.Model
    , fields : Fields.Model
    , parsing : Parsing.Model
    , formattingAndValidation : FormattingAndValidation.Model
    , customization : Customization.Model
    , internationalization : Internationalization.Model
    }


init : Model
init =
    { gettingStarted = GettingStarted.init
    , fields = Fields.init
    , parsing = Parsing.init
    , formattingAndValidation = FormattingAndValidation.init
    , customization = Customization.init
    , internationalization = Internationalization.init
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
                , Fields.chapter
                , Parsing.chapter
                , FormattingAndValidation.chapter
                , Customization.chapter
                , Internationalization.chapter
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
