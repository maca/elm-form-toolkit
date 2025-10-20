module Support.ExampleInputs exposing
    ( Band
    , BandFields(..)
    , FieldId(..)
    , Lang(..)
    , Person
    , bandFields
    , bandParser
    , blankInput
    , boolInput
    , checkboxInput
    , datetimeInput
    , floatInput
    , groupWithName
    , intInput
    , posixInput
    , radioInput
    , selectInput
    , stringInput
    , stringInputWithOptions
    )

import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Time


type FieldId
    = StringField
    | IntField
    | FloatField
    | BoolField
    | PosixField
    | DatetimeField
    | SelectField
    | BlankField


type Lang
    = ES
    | EN
    | DE


stringInput : Field FieldId
stringInput =
    Field.text
        [ Field.label "Enter your string"
        , Field.identifier StringField
        , Field.name "string-field"
        , Field.value (Value.string "A string")
        , Field.placeholder "String value"
        , Field.hint "Must be a string"
        , Field.class "styled-string-field"
        ]


stringInputWithOptions : Field FieldId
stringInputWithOptions =
    Field.text
        [ Field.label "Enter your string"
        , Field.identifier StringField
        , Field.name "string-field"
        , Field.value (Value.string "A string")
        , Field.placeholder "String value"
        , Field.hint "Must be a string"
        , Field.stringOptions [ "red", "green", "blue" ]
        ]


intInput : Field FieldId
intInput =
    Field.text
        [ Field.label "Enter your int"
        , Field.identifier IntField
        , Field.name "int-field"
        , Field.value (Value.int 1)
        ]


boolInput : Field FieldId
boolInput =
    Field.text
        [ Field.label "Enter your bool"
        , Field.identifier BoolField
        , Field.value (Value.bool True)
        ]


floatInput : Field FieldId
floatInput =
    Field.text
        [ Field.label "Enter your float"
        , Field.identifier FloatField
        , Field.value (Value.float 1.1)
        ]


posixInput : Field FieldId
posixInput =
    Field.text
        [ Field.label "Enter your posix"
        , Field.identifier PosixField
        , Field.value (Value.time (Time.millisToPosix 0))
        ]


datetimeInput : Field FieldId
datetimeInput =
    Field.datetime
        [ Field.label "Enter meeting time"
        , Field.identifier DatetimeField
        , Field.name "datetime-field"
        , Field.value (Value.time (Time.millisToPosix 1609459200000))
        ]


blankInput : Field FieldId
blankInput =
    Field.text
        [ Field.label "Enter nothing"
        , Field.name "blank-field"
        , Field.required True
        , Field.identifier BlankField
        ]


checkboxInput : Field id
checkboxInput =
    Field.checkbox
        [ Field.label "Accept"
        , Field.hint "You have to check the box"
        , Field.name "checkbox"
        , Field.required True
        ]


selectInput : Field FieldId
selectInput =
    Field.select
        [ Field.label "Language"
        , Field.hint "Select language"
        , Field.name "select"
        , Field.identifier SelectField
        , Field.required True
        , Field.stringOptions (List.map Tuple.first languages)
        ]


radioInput : Field id
radioInput =
    Field.radio
        [ Field.label "Radio inputs"
        , Field.name "radio-inputs"
        , Field.required True
        , Field.hint "Turn the light on or off"
        , Field.options
            [ ( "On", Value.bool True )
            , ( "Off", Value.bool False )
            ]
        ]


groupWithName : Field FieldId
groupWithName =
    Field.group [ Field.name "group" ] [ stringInput, intInput ]


type BandFields
    = BandMembers
    | BandName
    | MemberName
    | MemberAge


bandFields : Field BandFields
bandFields =
    Field.group []
        [ Field.text
            [ Field.label "Band Name"
            , Field.required True
            , Field.identifier BandName
            , Field.name "band-name"
            ]
        , Field.group
            [ Field.label "Members (max 5)" ]
            [ Field.repeatable
                [ Field.identifier BandMembers
                , Field.repeatableMin 1
                , Field.repeatableMax 5
                , Field.name "band-members"
                ]
                (Field.group []
                    [ Field.text
                        [ Field.label "Member Name"
                        , Field.identifier MemberName
                        , Field.name "member-name"
                        ]
                    , Field.int
                        [ Field.label "Member Age"
                        , Field.identifier MemberAge
                        , Field.name "member-age"
                        ]
                    ]
                )
                []
            ]
        ]


type alias Band =
    { name : String
    , members : List Person
    }


type alias Person =
    { name : String
    , age : Int
    }


bandParser : Parse.Parser BandFields Band
bandParser =
    Parse.map2 Band
        (Parse.field BandName Parse.string)
        (Parse.field BandMembers (Parse.list personParser))


personParser : Parse.Parser BandFields Person
personParser =
    Parse.map2 Person
        (Parse.field MemberName Parse.string)
        (Parse.field MemberAge Parse.int)


languages : List ( String, Lang )
languages =
    [ ( "Español", ES )
    , ( "English", EN )
    , ( "Deutsch", DE )
    ]
