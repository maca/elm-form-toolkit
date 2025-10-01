module Support.RepeatableDemo exposing (Model, Msg, init, update, view)

import Browser
import FormToolkit.Error as Error exposing (Error)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onSubmit)



-- TYPES


type alias Model =
    { formFields : Field RepeatableDemoFields
    , submitted : Bool
    , result : Result (List (Error RepeatableDemoFields)) RepeatableData
    }


type RepeatableDemoFields
    = EmailList
    | ContactList
    | ContactName
    | ContactPhone
    | ContactEmail
    | SkillsList


type Msg
    = FormChanged (Field.Msg RepeatableDemoFields)
    | FormSubmitted


type alias RepeatableData =
    { emails : List String
    , contacts : List Contact
    , skills : List String
    }


type alias Contact =
    { name : String
    , phone : String
    , email : String
    }



-- INIT


init : Model
init =
    { formFields = repeatableDemoForm
    , submitted = False
    , result = Err [ Error.CustomError Nothing "Waiting for input" ]
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged inputMsg ->
            let
                ( formFields, result ) =
                    Parse.parseUpdate repeatableParser inputMsg model.formFields
            in
            { formFields = formFields
            , result = result
            , submitted = False
            }

        FormSubmitted ->
            { model | submitted = True }



-- FORM DEFINITION


repeatableDemoForm : Field RepeatableDemoFields
repeatableDemoForm =
    Field.repeatable
        [ Field.label "Contact Information"
        , Field.identifier ContactList
        , Field.name "contact-list"
        , Field.repeatableMin 1
        , Field.repeatableMax 5
        , Field.copies
            { addFieldsButton = "Add Contact"
            , removeFieldsButton = "Remove Contact"
            }
        ]
        (Field.group []
            [ Field.text
                [ Field.label "Name"
                , Field.identifier ContactName
                , Field.required True
                ]
            , Field.text
                [ Field.label "Phone"
                , Field.identifier ContactPhone
                , Field.placeholder "Phone number"
                ]
            , Field.email
                [ Field.label "Email"
                , Field.identifier ContactEmail
                , Field.required True
                ]
            ]
        )
        []



-- PARSER


repeatableParser : Parse.Parser RepeatableDemoFields RepeatableData
repeatableParser =
    Parse.map3 RepeatableData
        (Parse.field EmailList (Parse.list Parse.string))
        (Parse.field ContactList (Parse.list contactParser))
        (Parse.field SkillsList (Parse.list Parse.string))


contactParser : Parse.Parser RepeatableDemoFields Contact
contactParser =
    Parse.map3 Contact
        (Parse.field ContactName Parse.string)
        (Parse.field ContactPhone Parse.string)
        (Parse.field ContactEmail Parse.string)



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "milligram"
        , Attr.style "margin-top" "20px"
        , Attr.style "padding" "20px"
        , Attr.style "border" "1px solid #d1d1d1"
        , Attr.style "border-radius" "4px"
        ]
        [ Html.h4 [] [ Html.text "Repeatable Fields Demo" ]
        , Html.p []
            [ Html.text "This demo shows different types of repeatable fields with various configurations and default values." ]
        , Html.form
            [ onSubmit FormSubmitted, Attr.novalidate True ]
            [ Field.toHtml FormChanged model.formFields
            , Html.button
                [ onClick FormSubmitted
                , Attr.style "margin-top" "1rem"
                ]
                [ Html.text "Submit" ]
            ]
        , if model.submitted then
            case model.result of
                Ok repeatableData ->
                    success [ Html.text "Form submitted successfully!" ]

                Err errors ->
                    failure [ Html.text "There are validation errors" ]

          else
            Html.text ""
        ]


success : List (Html msg) -> Html msg
success =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#e8f5e8"
        , Attr.style "border-radius" "4px"
        ]


failure : List (Html msg) -> Html msg
failure =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#fde8e8"
        , Attr.style "border-radius" "4px"
        ]
