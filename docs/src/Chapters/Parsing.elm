module Chapters.Parsing exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions exposing (updateStateWithCmdWith)
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html exposing (Html)
import Support.ShipmentForm as ShipmentForm


type alias Model =
    { contactDemo : ShipmentForm.Model
    , shipmentDemo : ShipmentForm.Model
    }


type alias Book book =
    { book | parsing : Model }


type Msg
    = ContactDemoMsg ShipmentForm.Msg
    | ShipmentDemoMsg ShipmentForm.Msg


init : Model
init =
    { contactDemo = ShipmentForm.init
    , shipmentDemo = ShipmentForm.init
    }


update : Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update msg state =
    case msg of
        ContactDemoMsg contactMsg ->
            ( { state
                | parsing =
                    { contactDemo = ShipmentForm.update contactMsg state.parsing.contactDemo
                    , shipmentDemo = state.parsing.shipmentDemo
                    }
              }
            , Cmd.none
            )

        ShipmentDemoMsg shipmentMsg ->
            ( { state
                | parsing =
                    { contactDemo = state.parsing.contactDemo
                    , shipmentDemo = ShipmentForm.update shipmentMsg state.parsing.shipmentDemo
                    }
              }
            , Cmd.none
            )


chapter : Chapter { x | parsing : Model }
chapter =
    Chapter.chapter "Parsing"
        |> Chapter.withStatefulComponentList
            [ ( "Basic Parsing - Contact Form"
              , \book ->
                    book.parsing.contactDemo
                        |> ShipmentForm.view
                        |> Html.map (ContactDemoMsg >> updateStateWithCmdWith update)
              )
            , ( "Advanced Parsing - Shipment Form"
              , \book ->
                    book.parsing.shipmentDemo
                        |> ShipmentForm.view
                        |> Html.map (ShipmentDemoMsg >> updateStateWithCmdWith update)
              )
            ]
        |> Chapter.render parsingIntroMarkdown


parsingIntroMarkdown : String
parsingIntroMarkdown =
    """

### Form Definition

```elm
type ContactFields
    = ContactName
    | ContactPassword

type alias Contact =
    { name : String
    , password : String
    }

contactForm : Field ContactFields
contactForm =
    Field.group []
        [ Field.text
            [ Field.label "Name"
            , Field.required True
            , Field.identifier ContactName
            , Field.name "contact-name"
            , Field.placeholder "Enter your name"
            ]
        , Field.password
            [ Field.label "Password"
            , Field.required True
            , Field.identifier ContactPassword
            , Field.name "contact-password"
            , Field.placeholder "Enter your password"
            ]
        ]
```

### Parser with map2

The parser uses `Parse.map2` to combine two field parsers into a Contact record:

```elm
contactParser : Parse.Parser ContactFields Contact
contactParser =
    Parse.map2 Contact
        (Parse.field ContactName Parse.string)
        (Parse.field ContactPassword Parse.string)
```

<component with-label="Basic Parsing - Contact Form"/>

## Advanced Parsing with andMap Pipeline

For more complex forms, we can use the `andMap` pipeline style, which scales well to many fields. This example shows a shipment form with nested groups and repeatable fields.

### Complex Form Structure

```elm
type alias Shipment =
    { shipping : Address
    , recipients : List Recipient
    }

type alias Address =
    { firstName : String
    , lastName : String
    , address : String
    , address2 : String
    , postalCode : String
    , state : String
    , country : Countries.Country
    }

type alias Recipient =
    { email : String
    , name : String
    }
```

### Parser with andMap Pipeline

The parser uses `Parse.succeed` and `Parse.andMap` to build a pipeline:

```elm
shipmentParser : Parse.Parser ShipmentFields Shipment
shipmentParser =
    Parse.succeed Shipment
        |> Parse.andMap addressParser
        |> Parse.andMap recipientsParser

addressParser : Parse.Parser ShipmentFields Address
addressParser =
    Parse.succeed Address
        |> Parse.andMap (Parse.field (AddressFields AddressFirstName) Parse.string)
        |> Parse.andMap (Parse.field (AddressFields AddressLastName) Parse.string)
        |> Parse.andMap (Parse.field (AddressFields AddressStreet) Parse.string)
        |> Parse.andMap (Parse.field (AddressFields Address2) Parse.string)
        |> Parse.andMap (Parse.field (AddressFields PostalCode) Parse.string)
        |> Parse.andMap (Parse.field (AddressFields AddressState) Parse.string)
        |> Parse.andMap (Parse.field (AddressFields AddressCountry) countryParser)

recipientsParser : Parse.Parser ShipmentFields (List Recipient)
recipientsParser =
    Parse.list recipientParser

recipientParser : Parse.Parser ShipmentFields Recipient
recipientParser =
    Parse.map2 Recipient
        (Parse.field (RecipientFields RecipientEmail) Parse.string)
        (Parse.field (RecipientFields RecipientName) Parse.string)
```

### Custom Parsing with andThen

For the country field, we use custom parsing with `Parse.andThen`:

```elm
countryParser : Parse.Parser ShipmentFields Countries.Country
countryParser =
    Parse.string
        |> Parse.andThen
            (\\countryCode ->
                case Countries.fromCode countryCode of
                    Just country ->
                        Parse.succeed country

                    Nothing ->
                        Parse.fail ("Invalid country code: " ++ countryCode)
            )
```

<component with-label="Advanced Parsing - Shipment Form"/>

"""
