module Chapters.GettingStarted exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions exposing (mapUpdate, updateStateWithCmdWith)
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
        DemoUpdated inputMsg ->
            let
                demo =
                    Demo.update inputMsg model.demo
            in
            ( { model | demo = demo }
            , Task.perform (ElmBook.Actions.logActionWithString "Result")
                (Task.succeed (Debug.toString demo.result))
            )


chapter : Chapter { x | gettingStarted : Model }
chapter =
    Chapter.chapter "Getting Started"
        |> Chapter.withStatefulComponentList
            [ ( "demo"
              , \s ->
                    s.gettingStarted.demo
                        |> Demo.view
                        |> Html.map
                            (updateStateWithCmdWith
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
# Welcome to elm-form-toolkit!

elm-form-toolkit is a comprehensive package for building, parsing, validating, and rendering forms in Elm. It provides a declarative API similar to Elm's Html for building complex forms with validation, custom types, and flexible rendering.

## Key Features:

- **Declarative form building** with an Html-like API
- **Built-in validation** with custom validation support  
- **Parse form data to custom types** using a Json.Decode-like interface
- **Flexible rendering** with customization options
- **Support for complex forms** with repeatable fields

Let's build a simple user registration form step by step to see how elm-form-toolkit works!
"""


stepOneMarkdown : String
stepOneMarkdown =
    """
## Step 1: Declare a Form

First, let's declare a simple form with two fields: name and email. We start by defining field identifiers and then create the form structure.

```elm
-- Define field identifiers
type UserFormFields
    = UserName
    | UserEmail

-- Declare the form
userForm : Field UserFormFields
userForm =
    Field.group []
        [ Field.text
            [ Field.label "Name"
            , Field.required True
            , Field.identifier UserName
            , Field.name "user-name"
            ]
        , Field.email
            [ Field.label "Email"
            , Field.required True
            , Field.identifier UserEmail
            , Field.name "user-email"
            ]
        ]
```

The form is built using `Field.group` to contain multiple fields, and each field has:
- **label**: Display text for the field
- **required**: Validation rule
- **identifier**: Used for parsing (connects to your custom type)
- **name**: HTML name attribute for the input
"""


stepTwoMarkdown : String
stepTwoMarkdown =
    """
## Step 2: Set up the Model

The form state should be kept in your model. This includes the form fields themselves, submission status, and the parsed result.

```elm
type alias Model =
    { formFields : Field UserFormFields
    , submitted : Bool
    , user : Maybe User
    }

type alias User =
    { name : String
    , email : String
    }

init : Model
init =
    { formFields = userForm
    , submitted = False
    , user = Nothing
    }
```

**Important**: The `formFields` hold the current state of all form inputs, including their values, validation states, and error messages. The `user` field will contain the parsed result when the form is valid.
"""


stepThreeMarkdown : String
stepThreeMarkdown =
    """
## Step 3: Handle Updates

Form updates are handled using `Parse.parseUpdate` along with a parser. Define your Msg type and update function:

```elm
type Msg
    = FormChanged (Field.Msg UserFormFields)
    | FormSubmitted

update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( formFields, result ) =
                    Parse.parseUpdate userParser inputMsg model.formFields
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
        (Parse.field UserName Parse.string)
        (Parse.field UserEmail Parse.string)
```

**Key points:**
- `Parse.parseUpdate` handles both updating the form state AND parsing the current form data
- The parser uses a **Json.Decode-like API** to extract and validate field values
- Field identifiers (`UserName`, `UserEmail`) connect your form fields to the parser
"""


stepFourMarkdown : String
stepFourMarkdown =
    """
## Step 4: Render the Form

Rendering the form is simple - use `Field.toHtml` with your Msg constructor:

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
## Complete Browser.sandbox Example

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

type UserFormFields = UserName | UserEmail

type Msg
    = FormChanged (Field.Msg UserFormFields)
    | FormSubmitted

type alias User = { name : String, email : String }

init : Model
init =
    { formFields = userForm, submitted = False, user = Nothing }

userForm : Field UserFormFields
userForm =
    Field.group []
        [ Field.text
            [ Field.label "Name", Field.required True
            , Field.identifier UserName, Field.name "user-name" ]
        , Field.email
            [ Field.label "Email", Field.required True
            , Field.identifier UserEmail, Field.name "user-email" ]
        ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged inputMsg ->
            let ( formFields, result ) =
                    Parse.parseUpdate userParser inputMsg model.formFields
            in { model | formFields = formFields, user = Result.toMaybe result }
        
        FormSubmitted ->
            { model | submitted = True }

userParser : Parse.Parser UserFormFields User
userParser =
    Parse.map2 User
        (Parse.field UserName Parse.string)
        (Parse.field UserEmail Parse.string)

view : Model -> Html Msg
view model =
    Html.form [ onSubmit FormSubmitted ]
        [ Field.toHtml FormChanged model.formFields
        , Html.button [ onClick FormSubmitted ] [ Html.text "Submit" ]
        ]
```

<component with-label="demo"/>
"""
