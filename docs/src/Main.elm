module Main exposing (main)

import Chapters.Customization as Customization
import Chapters.Fields as Fields
import Chapters.FormattingAndValidation as FormattingAndValidation
import Chapters.GettingStarted as GettingStarted
import Chapters.Internationalization as Internationalization
import Chapters.ParsingCookbook as ParsingCookbook
import Chapters.ParsingToJson as ParsingToJson
import ElmBook exposing (Book, book)
import ElmBook.Chapter as Chapter
import ElmBook.StatefulOptions as StatefulOptions


type alias Model =
    { gettingStarted : GettingStarted.Model
    , fields : Fields.Model
    , parsingCookbook : ParsingCookbook.Model
    , parsingToJson : ParsingToJson.Model
    , formattingAndValidation : FormattingAndValidation.Model
    , customization : Customization.Model
    , internationalization : Internationalization.Model
    }


init : Model
init =
    { gettingStarted = GettingStarted.init
    , fields = Fields.init
    , parsingCookbook = ParsingCookbook.init
    , parsingToJson = ParsingToJson.init
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
                , ParsingCookbook.chapter
                , ParsingToJson.chapter
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
