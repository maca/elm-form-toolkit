module Chapters.Parsing exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html
import Html.Attributes as Attr
import Json.Encode as Encode
import Support.ShipmentForm as ShipmentForm
import Support.ViewHelpers exposing (failureDiv, successDiv)
import Task


type alias Book book =
    { book | parsing : Model }


type Msg
    = PersonFormChanged (Field.Msg PersonFields)
    | ShipmentFormChanged ShipmentForm.Msg


type alias Model =
    { personForm : Field PersonFields
    , shipmentForm : ShipmentForm.Model
    }


type PersonFields
    = FirstName
    | LastName


init : Model
init =
    { personForm = personForm
    , shipmentForm = ShipmentForm.init
    }


update : Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update msg book =
    let
        model =
            book.parsing

        ( newModel, cmd ) =
            case msg of
                PersonFormChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate personParser fieldMsg model.personForm
                    in
                    ( { model | personForm = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                ShipmentFormChanged innerMsg ->
                    let
                        updatedShipmentForm =
                            ShipmentForm.update innerMsg model.shipmentForm
                    in
                    ( { model | shipmentForm = updatedShipmentForm }
                    , Cmd.none
                    )
    in
    ( { book | parsing = newModel }, cmd )


chapter : Chapter (Book book)
chapter =
    Chapter.chapter "Parsing"
        |> Chapter.withStatefulComponentList
            [ ( "Person Form (Custom Type ID)"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.parsing.personForm
                            |> Field.toHtml PersonFormChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Shipment Form (andMap Pipeline)"
              , \book ->
                    book.parsing.shipmentForm
                        |> ShipmentForm.view
                        |> Html.map (ShipmentFormChanged >> Actions.updateStateWithCmdWith update)
              )
            ]
        |> Chapter.render markdownContent


markdownContent : String
markdownContent =
    """

The parsing mechanism is meant to emulate `Json.Decode` closely, except for the
use of identifiers instead of json keys to reference fields.

The primitive parsers are `Parse.string`, `Parse.int`,
`Parse.float`, `Parse.bool`, `Parse.posix` for time, `Parse.list`, and `Parse.json`.
Use these in combination with `Parse.field` to apply to a field with a
corresponding identifier if it exists. If the field can have a blank value or be
not present use `Parse.maybe`.
`Parse.stringWithFormat` will format, validate and parse a text input with a
provided pattern.

The output of a parser can be transformed using `map`, and they can be combined
using `map2`, `map3`, and other map functions to create records or tuples. For
more complex scenarios, `andMap` can be used to apply parsers in sequence using
the applicative pattern, or `andThen` can be used to chain parsers where the second
depends on the first's result.

## Custom Type Identifiers

Identifiers are used to traverse fields, similar to using Json.Decode.field, they can
be anything, even a string, but the better approach is to use custom types as
identifiers.

Custom type identifiers mitigate the risk of typos by ensuring type constraints.
The compiler will catch any non-existing field references at compile time, making
the code more robust and preventing runtime errors.


```elm
type PersonFields
    = FirstName
    | LastName


personForm : Field PersonFields
personForm =
    Field.group
        [ Field.label "Person Information"
        , Field.class "inline-fields"
        ]
        [ Field.text
            [ Field.label "First Name"
            , Field.required True
            , Field.identifier FirstName
            ]
        , Field.text
            [ Field.label "Last Name"
            , Field.required True
            , Field.identifier LastName
            ]
        ]


personParser : Parse.Parser PersonFields { firstName : String, lastName : String }
personParser =
    Parse.map2
        (\\first last ->
            { firstName = first
            , lastName = last
            }
        )
        (Parse.field FirstName Parse.string)
        (Parse.field LastName Parse.string)
```

<component with-label="Person Form (Custom Type ID)"/>


## Parser Pipeline with `andMap`

For complex forms with multiple fields, use the applicative pattern with `andMap` to build parsing pipelines.

<component with-label="Shipment Form (andMap Pipeline)"/>

```elm
import Countries

type alias Address =
    { firstName : String
    , lastName : String
    , address : String
    , addressNumber : Int
    , addressExtra : Maybe String
    , postalCode : String
    , state : String
    , country : Countries.Country
    }

shipmentAddressParser : Parse.Parser ShipmentFields Address
shipmentAddressParser =
    Parse.succeed Address
        |> Parse.andMap (Parse.field AddressFirstName Parse.string)
        |> Parse.andMap (Parse.field AddressLastName Parse.string)
        |> Parse.andMap (Parse.field AddressStreet Parse.string)
        |> Parse.andMap (Parse.field AddressNumber Parse.int)
        |> Parse.andMap (Parse.field AddressExtra (Parse.maybe Parse.string))
        |> Parse.andMap (Parse.field PostalCode Parse.string)
        |> Parse.andMap (Parse.field AddressState Parse.string)
        |> Parse.andMap shipmentCountryParser
```

The `andMap` pattern works by applying each field parser to the constructor function. Each successful parse applies one argument to the constructor, building up the final record. If any field fails to parse, the entire parser fails with error information about the specific field.

For fields that require custom validation, use `andThen` to chain validation logic:

```elm
shipmentCountryParser : Parse.Parser ShipmentFields Countries.Country
shipmentCountryParser =
    Parse.field AddressCountry
        (Parse.string
            |> Parse.andThen
                (\\countryStr ->
                    case Countries.fromCode countryStr of
                        Just country ->
                            Parse.succeed country

                        Nothing ->
                            Parse.fail "Invalid country"
                )
        )
```

A full example of a sandbox application for this form can be found [here](https://github.com/maca/elm-form-toolkit/blob/main/docs/src/Support/ShipmentForm.elm).

"""


personForm : Field PersonFields
personForm =
    Field.group
        [ Field.label "Person Information"
        , Field.class "inline-fields"
        ]
        [ Field.text
            [ Field.label "First Name"
            , Field.required True
            , Field.identifier FirstName
            ]
        , Field.text
            [ Field.label "Last Name"
            , Field.required True
            , Field.identifier LastName
            ]
        ]


personParser : Parse.Parser PersonFields { firstName : String, lastName : String }
personParser =
    Parse.map2 (\first last -> { firstName = first, lastName = last })
        (Parse.field FirstName Parse.string)
        (Parse.field LastName Parse.string)


