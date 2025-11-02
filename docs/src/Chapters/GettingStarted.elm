module Chapters.GettingStarted exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Html
import Support.AddressForm as Demo
import Task


type alias Model =
    { demo : Demo.Model }


type Msg
    = DemoUpdated Demo.Msg


init : Model
init =
    { demo = Demo.init }


update : Msg -> Model -> ( Model, Cmd (ElmBook.Msg state) )
update msg model =
    case msg of
        DemoUpdated demoMsg ->
            let
                demo =
                    Demo.update demoMsg model.demo
            in
            ( { model | demo = demo }
            , Task.perform (Actions.logActionWithString "Demo")
                (Task.succeed "Press Submit to perform a request and see results")
            )


chapter : Chapter { x | gettingStarted : Model }
chapter =
    Chapter.chapter "Getting Started"
        |> Chapter.withStatefulComponentList
            [ ( "Demo"
              , \book ->
                    book.gettingStarted.demo
                        |> Demo.view
                        |> Html.map
                            (Actions.updateStateWithCmdWith
                                (\msg state ->
                                    update (DemoUpdated msg) state.gettingStarted
                                        |> Tuple.mapFirst
                                            (\gettingStarted ->
                                                { state | gettingStarted = gettingStarted }
                                            )
                                )
                            )
              )
            ]
        |> Chapter.render markdownContent



-- MARKDOWN CONTENT COMPONENTS


markdownContent : String
markdownContent =
    """
# Welcome to form-toolkit!

form-toolkit is a comprehensive package for building, parsing, validating, and
rendering forms in Elm.


## Features:

- Declarative and **opinionated** form field building with an Elm-Html-like API
- Built-in validation with custom validation support
- Parse form data to custom types using a Json.Decode-like interface
- Flexible rendering with customization options
- Support for complex forms with repeatable fields

Let's build an address form step by step to see how form-toolkit works!


<component with-label="Demo"/>


## Step 1: Declaring a Form

A `Field` represents all of the user inputs for a form and their corresponding
labels, hints and validation errors. It can be a single input field, a group of
Fields or a group of repeatable Fields.

Let's declare an address form with recipient information and address details.


```elm
-- Define field identifiers,
-- we will use this to refer to particular fields when parsing
type AddressFields
    = AddressNameGroup
    | AddressFirstName
    | AddressLastName
    | AddressStreet
    | AddressNumber
    | AddressExtra
    | LocalityGroup
    | PostalCode
    | AddressState
    | AddressCountry

-- Declare the form
addressFieldsDefinition : Field AddressFields
addressFieldsDefinition =
    Field.group
        []
        [ Field.group
            [ Field.label "Recipient"
            , Field.name "recipient"
            , Field.identifier AddressNameGroup
            , Field.class "inline-fields"
            ]
            [ Field.text
                [ Field.label "First Name"
                , Field.identifier AddressFirstName
                , Field.name "first-name"
                , Field.required True
                ]
            , Field.text
                [ Field.label "Last Name"
                , Field.identifier AddressLastName
                , Field.name "last-name"
                , Field.required True
                ]
            ]
        , Field.group
            [ Field.name "address"
            , Field.label "Address"
            ]
            [ Field.group
                [ Field.class "inline-fields" ]
                [ Field.text
                    [ Field.label "Street Name"
                    , Field.class "column column-75"
                    , Field.required True
                    , Field.identifier AddressStreet
                    , Field.name "street-name"
                    ]
                , Field.text
                    [ Field.label "Street Number"
                    , Field.identifier AddressNumber
                    , Field.required True
                    , Field.name "address-number"
                    ]
                ]
            , Field.text
                [ Field.label "Address 2"
                , Field.identifier AddressExtra
                , Field.name "address-2"
                ]
            , Field.group
                [ Field.identifier LocalityGroup
                , Field.class "locality"
                , Field.class "inline-fields"
                ]
                [ Field.text
                    [ Field.label "Postal code"
                    , Field.required True
                    , Field.identifier PostalCode
                    , Field.name "postal-code"
                    ]
                , Field.text
                    [ Field.label "State"
                    , Field.required True
                    , Field.identifier AddressState
                    , Field.name "state"
                    ]
                , Field.select
                    [ Field.label "Country"
                    , Field.required True
                    , Field.identifier AddressCountry
                    , Field.name "country"
                    , Field.options
                        (Countries.all
                            |> List.map
                                (\\country ->
                                    ( country.name ++ " " ++ country.flag
                                    , Value.string country.code
                                    )
                                )
                        )
                    ]
                ]
            ]
        ]
```

Each field has:
- **label**: Display text for the field
- **required**: Validate the presence of input value
- **identifier**: Used for parsing (connects to your custom type)
- **name**: HTML name attribute for the input
- **class**: CSS classes for styling
- **options**: Available choices for select fields


## Step 2: Setting up the Model


The API was modeled after the Html API with a list of attributes and a list
of children nodes, but once a Field is built it should be kept in the model
because it keeps track of the Fields state, and validation errors.


```elm
type alias Model =
    { addressFields : Field AddressFields
    , submitted : Bool
    , result : Maybe (Result (Error AddressFields) Address)
    }

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

init : Model
init =
    { addressFields = addressFieldsDefinition
    , submitted = False
    , result = Nothing
    }
```


## Step 3: Handle Updates

Form updates are handled using `Parse.parseUpdate` along with a parser.
Define your Msg type and update function:


```elm
type Msg
    = FormChanged (Field.Msg AddressFields)
    | FormSubmitted

update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( addressFields, result ) =
                    Parse.parseUpdate addressParser inputMsg model.addressFields
            in
            { model
                | addressFields = addressFields
                , result = Just result
            }

        FormSubmitted ->
            case Parse.parseValidate Parse.json model.addressFields of
                ( updatedField, Ok jsonValue ) ->
                    { model
                        | addressFields = updatedField
                        , submitted = True
                        , result = Nothing
                    }

                ( updatedField, Err _ ) ->
                    { model | addressFields = updatedField }

-- Parser to convert form data to Address type
addressParser : Parse.Parser AddressFields Address
addressParser =
    Parse.succeed Address
        |> Parse.andMap (Parse.field AddressFirstName Parse.string)
        |> Parse.andMap (Parse.field AddressLastName Parse.string)
        |> Parse.andMap (Parse.field AddressStreet Parse.string)
        |> Parse.andMap (Parse.field AddressNumber Parse.int)
        |> Parse.andMap (Parse.field AddressExtra (Parse.maybe Parse.string))
        |> Parse.andMap (Parse.field PostalCode Parse.string)
        |> Parse.andMap (Parse.field AddressState Parse.string)
        |> Parse.andMap countryParser

countryParser : Parse.Parser AddressFields Countries.Country
countryParser =
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

**Key points:**
- `Parse.parseUpdate` handles both updating the form state AND parsing the current form data
- Field identifiers are used to reference specific fields while parsing, similar to using `field` in `Json.Decode`
- `Parse.andMap` allows building parsers in a pipeline style for records with many fields
- `Parse.andThen` enables custom validation logic (like validating the country code)
- `Parse.maybe` creates optional fields that parse to `Maybe` values
- `Parse.int` parses string input to integers with validation



## Step 4: Render the Form

Use `Field.toHtml` passing a `Msg` constructor, used to tag form messages.
Use Elm HTML markup to wrap the fields in a form and bind actions to form submit
and submit button click.


```elm
view : Model -> Html Msg
view model =
    Html.form
        [ onSubmit FormSubmitted ]
        [ Field.toHtml FormChanged model.addressFields
        , Html.button
            [ onClick FormSubmitted ]
            [ Html.text "Submit" ]
        ]
```

`Field.toHtml` automatically renders:
- **All form fields** with proper HTML structure
- **Labels** for each field
- **Input elements** with appropriate types
- **Validation errors** when fields are invalid
- **Required** class for mandatory fields
- **Nested field groups** with proper semantic structure


## Complete Example

A full example of a sandbox application for this form can be found
[here](https://github.com/maca/form-toolkit/blob/main/docs/src/Support/AddressForm.elm).

"""
