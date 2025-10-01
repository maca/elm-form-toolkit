module Chapters.FieldReference exposing (Model, Msg, chapter, init)

import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html
import Html.Attributes as Attr
import Support.RepeatableDemo as RepeatableDemo
import Task


type alias Book book =
    { book | fieldReference : Model }


type alias Model =
    { text : Field ()
    , textarea : Field ()
    , email : Field ()
    , password : Field ()
    , autocompleteText : Field ()
    , autocomplete : Field ()
    , int : Field ()
    , float : Field ()
    , date : Field ()
    , month : Field ()
    , select : Field ()
    , radio : Field ()
    , checkbox : Field ()
    , group : Field String
    , repeatableDemo : RepeatableDemo.Model
    }


type Msg
    = TextChanged (Field.Msg ())
    | TextareaChanged (Field.Msg ())
    | EmailChanged (Field.Msg ())
    | PasswordChanged (Field.Msg ())
    | AutocompleteTextChanged (Field.Msg ())
    | AutocompleteChanged (Field.Msg ())
    | IntChanged (Field.Msg ())
    | FloatChanged (Field.Msg ())
    | DateChanged (Field.Msg ())
    | MonthChanged (Field.Msg ())
    | SelectChanged (Field.Msg ())
    | RadioChanged (Field.Msg ())
    | CheckboxChanged (Field.Msg ())
    | GroupChanged (Field.Msg String)
    | RepeatableDemoUpdated RepeatableDemo.Msg


init : Model
init =
    { text = textField
    , textarea = textareaField
    , email = emailField
    , password = passwordField
    , autocompleteText = autocompleteTextField
    , autocomplete = autocompleteField
    , int = intField
    , float = floatField
    , date = dateField
    , month = monthField
    , select = selectField
    , radio = radioField
    , checkbox = checkboxField
    , group = groupField
    , repeatableDemo = RepeatableDemo.init
    }


textField : Field ()
textField =
    Field.text
        [ Field.label "Text Field"
        , Field.placeholder "Enter any text"
        , Field.required True
        ]


textareaField : Field ()
textareaField =
    Field.textarea
        [ Field.label "Textarea Field"
        , Field.placeholder "Enter multiple lines of text"
        , Field.autogrow True
        , Field.required True
        ]


emailField : Field ()
emailField =
    Field.email
        [ Field.label "Email Field"
        , Field.placeholder "your@email.com"
        , Field.required True
        ]


passwordField : Field ()
passwordField =
    Field.password
        [ Field.label "Password Field"
        , Field.placeholder "Enter password"
        , Field.required True
        ]


autocompleteTextField : Field ()
autocompleteTextField =
    Field.text
        [ Field.label "Text with suggestions"
        , Field.placeholder "Type to see suggestions"
        , Field.stringOptions [ "Apple", "Banana", "Cherry", "Date", "Elderberry", "Fig" ]
        , Field.required True
        ]


autocompleteField : Field ()
autocompleteField =
    Field.strictAutocomplete
        [ Field.label "Strict Autocomplete"
        , Field.placeholder "Choose from options"
        , Field.stringOptions [ "Apple", "Banana", "Cherry", "Date" ]
        , Field.required True
        ]


intField : Field ()
intField =
    Field.int
        [ Field.label "Integer Field"
        , Field.placeholder "Enter a whole number"
        , Field.min (Value.int 0)
        , Field.max (Value.int 100)
        , Field.step (Value.int 5)
        , Field.required True
        ]


floatField : Field ()
floatField =
    Field.float
        [ Field.label "Float Field"
        , Field.placeholder "Enter a decimal number"
        , Field.min (Value.float 0.0)
        , Field.max (Value.float 10.0)
        , Field.step (Value.float 0.1)
        , Field.required True
        ]


dateField : Field ()
dateField =
    Field.date
        [ Field.label "Date Field"
        , Field.required True
        ]


monthField : Field ()
monthField =
    Field.month
        [ Field.label "Month Fiend"
        , Field.required True
        ]


selectField : Field ()
selectField =
    Field.select
        [ Field.label "Select Field"
        , Field.required True
        , Field.options
            [ ( "Small", Value.string "small" )
            , ( "Medium", Value.string "medium" )
            , ( "Large", Value.string "large" )
            , ( "Extra Large", Value.string "xlarge" )
            ]
        ]


radioField : Field ()
radioField =
    Field.radio
        [ Field.label "Radio Field"
        , Field.required True
        , Field.options
            [ ( "Red", Value.string "red" )
            , ( "Green", Value.string "green" )
            , ( "Blue", Value.string "blue" )
            ]
        ]


checkboxField : Field ()
checkboxField =
    Field.checkbox
        [ Field.label "Checkbox Field"
        ]


groupField : Field String
groupField =
    Field.group
        [ Field.label "Contact Information"
        , Field.class "contact-fields"
        ]
        [ Field.text
            [ Field.label "Name"
            , Field.placeholder "Enter your name"
            , Field.required True
            , Field.identifier "name"
            ]
        , Field.email
            [ Field.label "Email"
            , Field.placeholder "your@email.com"
            , Field.required True
            , Field.identifier "email"
            ]
        ]


update : Msg -> Book book -> ( Book book, Cmd (ElmBook.Msg (Book book)) )
update msg book =
    let
        model =
            book.fieldReference

        ( newModel, cmd ) =
            case msg of
                TextChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.text
                    in
                    ( { model | text = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                TextareaChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.textarea
                    in
                    ( { model | textarea = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                EmailChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.email
                    in
                    ( { model | email = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                PasswordChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.password
                    in
                    ( { model | password = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                AutocompleteTextChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.autocompleteText
                    in
                    ( { model | autocompleteText = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                AutocompleteChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.autocomplete
                    in
                    ( { model | autocomplete = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                IntChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.int fieldMsg model.int
                    in
                    ( { model | int = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                FloatChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.float fieldMsg model.float
                    in
                    ( { model | float = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                DateChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.posix fieldMsg model.date
                    in
                    ( { model | date = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                MonthChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.posix fieldMsg model.month
                    in
                    ( { model | month = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                SelectChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.select
                    in
                    ( { model | select = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                RadioChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.string fieldMsg model.radio
                    in
                    ( { model | radio = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                CheckboxChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate Parse.bool fieldMsg model.checkbox
                    in
                    ( { model | checkbox = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                GroupChanged fieldMsg ->
                    let
                        ( updatedField, result ) =
                            Parse.parseUpdate
                                (Parse.map2 (\name email -> { name = name, email = email })
                                    (Parse.field "name" Parse.string)
                                    (Parse.field "email" Parse.string)
                                )
                                fieldMsg
                                model.group
                    in
                    ( { model | group = updatedField }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString result))
                    )

                RepeatableDemoUpdated repeatableMsg ->
                    let
                        repeatableDemo =
                            RepeatableDemo.update repeatableMsg model.repeatableDemo
                    in
                    ( { model | repeatableDemo = repeatableDemo }
                    , Task.perform (Actions.logActionWithString "Result")
                        (Task.succeed (Debug.toString repeatableDemo.result))
                    )
    in
    ( { book | fieldReference = newModel }, cmd )


chapter : Chapter (Book book)
chapter =
    Chapter.chapter "Field Reference"
        |> Chapter.withStatefulComponentList
            [ ( "Text"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.text
                            |> Field.toHtml TextChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Textarea"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.textarea
                            |> Field.toHtml TextareaChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Email"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.email
                            |> Field.toHtml EmailChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Password"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.password
                            |> Field.toHtml PasswordChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Text with suggestions"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.autocompleteText
                            |> Field.toHtml AutocompleteTextChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Autocomplete"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.autocomplete
                            |> Field.toHtml AutocompleteChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Int"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.int
                            |> Field.toHtml IntChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Float"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.float
                            |> Field.toHtml FloatChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Date"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.date
                            |> Field.toHtml DateChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Month"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.month
                            |> Field.toHtml MonthChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Select"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.select
                            |> Field.toHtml SelectChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Radio"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.radio
                            |> Field.toHtml RadioChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Checkbox"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.checkbox
                            |> Field.toHtml CheckboxChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Group"
              , \book ->
                    Html.div [ Attr.class "milligram" ]
                        [ book.fieldReference.group
                            |> Field.toHtml GroupChanged
                        ]
                        |> Html.map (Actions.updateStateWithCmdWith update)
              )
            , ( "Repeatable Demo"
              , \book ->
                    book.fieldReference.repeatableDemo
                        |> RepeatableDemo.view
                        |> Html.map (Actions.updateStateWithCmdWith (update << RepeatableDemoUpdated))
              )
            ]
        |> Chapter.render
            (String.concat
                [ inputTypesMarkdown
                , repeatableFieldsMarkdown
                , repeatableAdvancedMarkdown
                ]
            )



-- MARKDOWN CONTENT COMPONENTS


inputTypesMarkdown : String
inputTypesMarkdown =
    """

### Text

Basic text input. Use `Field.placeholder` for user guidance and `Field.required` to enforce validation.

```elm
textField : Field ()
textField =
    Field.text
        [ Field.label "Text Field"
        , Field.placeholder "Enter any text"
        , Field.required True
        ]
```

<component with-label="Text"/>

### Textarea

Multi-line text input. Use `Field.autogrow` to automatically expand the height as content grows.

```elm
textareaField : Field ()
textareaField =
    Field.textarea
        [ Field.label "Textarea Field"
        , Field.placeholder "Enter multiple lines of text"
        , Field.autogrow True
        , Field.required True
        ]
```

<component with-label="Textarea"/>

### Email

Email validation with proper input type and built-in validation.

```elm
emailField : Field ()
emailField =
    Field.email
        [ Field.label "Email Field"
        , Field.placeholder "your@email.com"
        , Field.required True
        ]
```

<component with-label="Email"/>

### Password

Masked input for sensitive data with security features.

```elm
passwordField : Field ()
passwordField =
    Field.password
        [ Field.label "Password Field"
        , Field.placeholder "Enter password"
        , Field.required True
        ]
```

<component with-label="Password"/>

### Text with Suggestions

Text input that shows suggestions while allowing free text entry. Use `Field.stringOptions` to provide a list of suggestions.

```elm
autocompleteTextField : Field ()
autocompleteTextField =
    Field.text
        [ Field.label "Text with suggestions"
        , Field.placeholder "Type to see suggestions"
        , Field.stringOptions [ "Apple", "Banana", "Cherry", "Date", "Elderberry", "Fig" ]
        , Field.required True
        ]
```

<component with-label="Text with suggestions"/>

### Strict Autocomplete

Strict autocomplete that only allows selection from predefined options. Use `Field.strictAutocomplete` instead of `Field.text`.

```elm
autocompleteField : Field ()
autocompleteField =
    Field.strictAutocomplete
        [ Field.label "Strict Autocomplete"
        , Field.placeholder "Choose from options"
        , Field.stringOptions [ "Apple", "Banana", "Cherry", "Date" ]
        , Field.required True
        ]
```

<component with-label="Autocomplete"/>

### Integer

Integer input with number validation. Use `Field.min` and `Field.max` to set validation boundaries and `Field.step` to control increment steps for numeric values.

```elm
intField : Field ()
intField =
    Field.int
        [ Field.label "Integer Field"
        , Field.placeholder "Enter a whole number"
        , Field.min (Value.int 0)
        , Field.max (Value.int 100)
        , Field.step (Value.int 5)
        , Field.required True
        ]
```

<component with-label="Int"/>

### Float

Decimal number input with precision control and validation. Use `Value.float` for floating-point boundaries.

```elm
floatField : Field ()
floatField =
    Field.float
        [ Field.label "Float Field"
        , Field.placeholder "Enter a decimal number"
        , Field.min (Value.float 0.0)
        , Field.max (Value.float 10.0)
        , Field.step (Value.float 0.1)
        , Field.required True
        ]
```

<component with-label="Float"/>

### Date

Date picker with built-in validation and date formatting.

```elm
dateField : Field ()
dateField =
    Field.date
        [ Field.label "Date Field"
        , Field.required True
        ]
```

<component with-label="Date"/>

### Month

Month/year selection with proper validation.

```elm
monthField : Field ()
monthField =
    Field.month
        [ Field.label "Month Field"
        , Field.required True
        ]
```

<component with-label="Month"/>

### Select

Dropdown selection from predefined options. Use `Field.options` with tuples of display text and `Value.string` for the underlying values.

```elm
selectField : Field ()
selectField =
    Field.select
        [ Field.label "Select Field"
        , Field.required True
        , Field.options
            [ ( "Small", Value.string "small" )
            , ( "Medium", Value.string "medium" )
            , ( "Large", Value.string "large" )
            , ( "Extra Large", Value.string "xlarge" )
            ]
        ]
```

<component with-label="Select"/>

### Radio

Single selection from visible options with proper grouping.

```elm
radioField : Field ()
radioField =
    Field.radio
        [ Field.label "Radio Field"
        , Field.required True
        , Field.options
            [ ( "Red", Value.string "red" )
            , ( "Green", Value.string "green" )
            , ( "Blue", Value.string "blue" )
            ]
        ]
```

<component with-label="Radio"/>

### Checkbox

Boolean input for yes/no values with proper accessibility.

```elm
checkboxField : Field ()
checkboxField =
    Field.checkbox
        [ Field.label "Checkbox Field"
        ]
```

<component with-label="Checkbox"/>

### Group

Group multiple related fields together with shared validation and styling. Use `Field.label` to set the legend text for the group.

```elm
groupField : Field String
groupField =
    Field.group
        [ Field.label "Contact Information"
        , Field.class "contact-fields"
        ]
        [ Field.text
            [ Field.label "Name"
            , Field.placeholder "Enter your name"
            , Field.required True
            , Field.identifier "name"
            ]
        , Field.email
            [ Field.label "Email"
            , Field.placeholder "your@email.com"
            , Field.required True
            , Field.identifier "email"
            ]
        ]
```

<component with-label="Group"/>
"""


repeatableFieldsMarkdown : String
repeatableFieldsMarkdown =
    """
## Repeatable Fields

Repeatable fields allow users to dynamically add and remove field instances, perfect for collecting lists of data like multiple addresses, phone numbers, or any collection-based input.

<component with-label="Repeatable Demo"/>

### Key Features:

- **Dynamic Addition/Removal**: Users can add or remove field instances
- **Validation**: Each instance is validated independently
- **Minimum/Maximum Limits**: Control how many instances are allowed
- **Default Values**: Pre-populate with initial data
"""


repeatableAdvancedMarkdown : String
repeatableAdvancedMarkdown =
    """
### Repeatable Configuration Options

The `repeatable` function provides several configuration options:

- **`repeatableMin`**: Minimum number of instances (default: 0)
- **`repeatableMax`**: Maximum number of instances (default: unlimited)
- **`copies`**: Initial number of empty instances to display

### Setting Default Values

You can provide default values for repeatable fields by using `updateAttribute` with a list of initial values. This is particularly useful when editing existing data:

```elm
-- Example: Pre-populate with existing phone numbers
phoneNumbersField =
    Field.repeatable []
        phoneNumberField
        |> Field.updateAttribute 
            [ "555-0123"
            , "555-0456" 
            ]
```

The `updateAttribute` function accepts a list where each item becomes the initial value for a repeatable instance. This allows you to load existing data seamlessly into the form.

### Use Cases

Repeatable fields are ideal for:
- **Contact Information**: Multiple phone numbers, addresses, emails
- **Skills/Tags**: Dynamic lists of user skills or tags
- **Line Items**: Shopping cart items, invoice lines
- **Family Members**: Collecting information about multiple people
- **Social Links**: Multiple social media profiles
"""
