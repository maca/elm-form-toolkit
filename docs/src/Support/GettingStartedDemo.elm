module Support.GettingStartedDemo exposing (Model, Msg, init, update, view)

import Browser
import FormToolkit.Error as Error exposing (Error)
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import Html exposing (Html)
import Html.Attributes as Attr exposing (novalidate)
import Html.Events exposing (onClick, onSubmit)
import Result



-- TYPES


type alias Model =
    { formFields : Field UserFormFields
    , submitted : Bool
    , result : Result (List (Error UserFormFields)) User
    }


type UserFormFields
    = UserName
    | UserEmail


type Msg
    = FormChanged (Field.Msg UserFormFields)
    | FormSubmitted


type alias User =
    { name : String
    , email : String
    }



-- INIT


init : Model
init =
    { formFields = userForm
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
                    Parse.parseUpdate userParser inputMsg model.formFields
            in
            { formFields = formFields
            , result = result
            , submitted = False
            }

        FormSubmitted ->
            { model | submitted = True }



-- FORM DEFINITION


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



-- PARSER


userParser : Parse.Parser UserFormFields User
userParser =
    Parse.map2 User
        (Parse.field UserName Parse.string)
        (Parse.field UserEmail Parse.string)



-- VIEW FOR DEMO COMPONENT


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "milligram"
        , Attr.style "margin-top" "20px"
        , Attr.style "padding" "20px"
        , Attr.style "border" "1px solid #d1d1d1"
        , Attr.style "border-radius" "4px"
        ]
        [ Html.h4 [] [ Html.text "Try the Form" ]
        , Html.form
            [ onSubmit FormSubmitted, novalidate True ]
            [ Field.toHtml FormChanged model.formFields
            , Html.button
                [ onClick FormSubmitted
                , Attr.style "margin-top" "1rem"
                ]
                [ Html.text "Submit" ]
            ]
        , if model.submitted then
            case model.result of
                Ok user ->
                    success
                        [ Html.div
                            []
                            [ Html.text "Form submitted successfully!" ]
                        , Html.div
                            []
                            [ Html.text
                                ("Parsed User: " ++ user.name ++ " (" ++ user.email ++ ")")
                            ]
                        ]

                Err _ ->
                    failure
                        [ Html.text "There are some errors" ]

          else
            Html.text ""
        ]


success =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#e8f5e8"
        , Attr.style "border-radius" "4px"
        ]


failure =
    Html.div
        [ Attr.style "margin-top" "1rem"
        , Attr.style "padding" "1rem"
        , Attr.style "background" "#fde8e8"
        , Attr.style "border-radius" "4px"
        ]



-- BROWSER.SANDBOX PROGRAM (for standalone usage)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
