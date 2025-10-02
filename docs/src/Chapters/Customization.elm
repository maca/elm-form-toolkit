module Chapters.Customization exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions exposing (updateStateWithCmdWith)
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html exposing (Html)
import Support.ShipmentForm as ShipmentForm


chapter : Chapter { x | customization : Model }
chapter =
    Chapter.chapter "Customization"
        |> Chapter.withStatefulComponent
            (\{ customization } ->
                view customization
                    |> Html.map (updateStateWithCmdWith update)
            )
        |> Chapter.renderWithComponentList ""


type alias Book book =
    { book | customization : Model }


type alias Model =
    { shipmentDemo : ShipmentForm.Model
    }


type Msg
    = ShipmentDemoMsg ShipmentForm.Msg


init : Model
init =
    { shipmentDemo = ShipmentForm.init
    }


update : Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update msg state =
    case msg of
        ShipmentDemoMsg shipmentMsg ->
            ( { state
                | customization =
                    { shipmentDemo = ShipmentForm.update shipmentMsg state.customization.shipmentDemo
                    }
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.h1 [] [ Html.text "Customization" ]
        , model.shipmentDemo
            |> ShipmentForm.view
            |> Html.map ShipmentDemoMsg
        ]
