module Chapters.GettingStarted exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import Html exposing (Html)
import Html.Attributes as Attr exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)
import Result
import Support.GettingStartedDemo as Demo
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
            , Task.perform (Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString demo.result))
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
        |> Chapter.render
            (String.join ""
                [ introMarkdown
                , stepOneMarkdown
                , stepTwoMarkdown
                , stepThreeMarkdown
                , stepFourMarkdown
                , completeExampleMarkdown
                ]
            )



-- MARKDOWN CONTENT COMPONENTS


introMarkdown : String
introMarkdown =
    """
# Welcome to form-toolkit!

form-toolkit is a comprehensive package for building, parsing, validating, and
rendering forms in Elm for building complex forms with validation, parsing to
any shape, and flexible rendering.


## Key Features:

- Declarative and opinionated form building with an Elm-Html-like API
- Built-in validation with custom validation support  
- Parse form data to custom types using a Json.Decode-like interface
- Flexible rendering with customization options
- Support for complex forms with repeatable fields

Let's build a simple name form step by step to see how form-toolkit works!
"""


stepOneMarkdown : String
stepOneMarkdown =
    """
## Step 1: Declaring a Form

First, let's declare a simple form with two fields: first name and last name. We start by
defining field identifiers and then create the form structure.


```elm
-- Define field identifiers
type UserFormFields
    = FirstName
    | LastName

-- Declare the form
userForm : Field UserFormFields
userForm =
    Field.group []
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
```

Each field has:
- **label**: Display text for the field
- **required**: Validate the presence of input value.
- **identifier**: Used for parsing (connects to your custom type)


"""


stepTwoMarkdown : String
stepTwoMarkdown =
    """
## Step 2: Setting up the Model

The form should be kept in your model, this keeps track of inputs and values,
and validation errors, nothing else, so it's safe to keep in the model.



```elm
type alias Model =
    { formFields : Field UserFormFields
    , submitted : Bool
    , user : Maybe User
    }

type alias User =
    { firstName : String
    , lastName : String
    }

init : Model
init =
    { formFields = userForm
    , submitted = False
    , user = Nothing
    }
```

"""


stepThreeMarkdown : String
stepThreeMarkdown =
    """
## Step 3: Handle Updates

Form updates are handled using `Parse.parseUpdate` along with a parser.
Define your Msg type and update function:


```elm
type Msg
    = FormChanged (Field.Msg UserFormFields)
    | FormSubmitted

update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged fieldMsg ->
            let
                ( formFields, result ) =
                    Parse.parseUpdate userParser fieldMsg model.formFields
            in
            { model
                | formFields = formFields
                , user = Result.toMaybe result
            }

        FormSubmitted ->
            { model | submitted = True }

-- Parser to convert form data to User type
userParser : Parse.Parser UserFormFields User
userParser =
    Parse.map2 User
        (Parse.field FirstName Parse.string)
        (Parse.field LastName Parse.string)
```

**Key points:**
- `Parse.parseUpdate` handles both updating the form state AND parsing the current form data
- Field identifiers `FirstName` and `LastName` are used to reference fields while parsing,
similar as using `field` when using `Json.Decode`
"""


stepFourMarkdown : String
stepFourMarkdown =
    """
## Step 4: Render the Form

Use `Field.toHtml` with your `Msg` constructor:

```elm
view : Model -> Html Msg
view model =
    Html.form
        [ onSubmit FormSubmitted ]
        [ Field.toHtml FormChanged model.formFields
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
- **Required indicators** for mandatory fields

The first argument is a function that wraps `Field.Msg` in your app's `Msg` type.
"""


completeExampleMarkdown : String
completeExampleMarkdown =
    """
## Complete Example

Here's the complete working example that you can copy and use:

```elm
import Browser
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import Html exposing (Html)
import Html.Events exposing (onClick, onSubmit)

main =
    Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    { formFields : Field UserFormFields
    , submitted : Bool
    , user : Maybe User
    }

type UserFormFields = FirstName | LastName

type Msg
    = FormChanged (Field.Msg UserFormFields)
    | FormSubmitted

type alias User = { firstName : String, lastName : String }

init : Model
init =
    { formFields = userForm, submitted = False, user = Nothing }

userForm : Field UserFormFields
userForm =
    Field.group []
        [ Field.text
            [ Field.label "First Name", Field.required True
            , Field.identifier FirstName, Field.name "first-name" ]
        , Field.text
            [ Field.label "Last Name", Field.required True
            , Field.identifier LastName, Field.name "last-name" ]
        ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged fieldMsg ->
            let ( formFields, result ) =
                    Parse.parseUpdate userParser fieldMsg model.formFields
            in { model | formFields = formFields, user = Result.toMaybe result }
        
        FormSubmitted ->
            { model | submitted = True }

userParser : Parse.Parser UserFormFields User
userParser =
    Parse.map2 User
        (Parse.field FirstName Parse.string)
        (Parse.field LastName Parse.string)

view : Model -> Html Msg
view model =
    Html.form [ onSubmit FormSubmitted ]
        [ Field.toHtml FormChanged model.formFields
        , Html.button [ onClick FormSubmitted ] [ Html.text "Submit" ]
        ]
```

<component with-label="Demo"/>
"""
