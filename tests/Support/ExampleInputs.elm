module Support.ExampleInputs exposing
    ( Band
    , BandFields(..)
    , FieldId(..)
    , Lang(..)
    , Person
    , bandDecoder
    , bandFields
    , blankInput
    , boolInput
    , checkboxInput
    , floatInput
    , groupWithName
    , groupWithNoName
    , intInput
    , posixInput
    , radioInput
    , selectInput
    , stringInput
    , stringInputWithOptions
    )

import FormToolkit.Field as Input exposing (Field)
import FormToolkit.Parse as Decode
import FormToolkit.Value as Value
import Time


type FieldId
    = StringField
    | IntField
    | FloatField
    | BoolField
    | PosixField
    | SelectField
    | BlankField


type Lang
    = ES
    | EN
    | DE


stringInput : Field FieldId val
stringInput =
    Input.text
        [ Input.label "Enter your string"
        , Input.identifier StringField
        , Input.name "string-field"
        , Input.value (Value.string "A string")
        , Input.placeholder "String value"
        , Input.hint "Must be a string"
        ]


stringInputWithOptions : Field FieldId val
stringInputWithOptions =
    Input.text
        [ Input.label "Enter your string"
        , Input.identifier StringField
        , Input.name "string-field"
        , Input.value (Value.string "A string")
        , Input.placeholder "String value"
        , Input.hint "Must be a string"
        , Input.stringOptions [ "red", "green", "blue" ]
        ]


intInput : Field FieldId val
intInput =
    Input.text
        [ Input.label "Enter your int"
        , Input.identifier IntField
        , Input.name "int-field"
        , Input.value (Value.int 1)
        ]


boolInput : Field FieldId val
boolInput =
    Input.text
        [ Input.label "Enter your bool"
        , Input.identifier BoolField
        , Input.value (Value.bool True)
        ]


floatInput : Field FieldId val
floatInput =
    Input.text
        [ Input.label "Enter your float"
        , Input.identifier FloatField
        , Input.value (Value.float 1.1)
        ]


posixInput : Field FieldId val
posixInput =
    Input.text
        [ Input.label "Enter your posix"
        , Input.identifier PosixField
        , Input.value (Value.time (Time.millisToPosix 0))
        ]


blankInput : Field FieldId val
blankInput =
    Input.text
        [ Input.label "Enter nothing"
        , Input.name "blank-field"
        , Input.required True
        , Input.identifier BlankField
        ]


checkboxInput : Field id val
checkboxInput =
    Input.checkbox
        [ Input.label "Accept"
        , Input.hint "You have to check the box"
        , Input.name "checkbox"
        , Input.required True
        ]


selectInput : Field FieldId Lang
selectInput =
    Input.select
        [ Input.label "Language"
        , Input.hint "Select language"
        , Input.name "select"
        , Input.identifier SelectField
        , Input.required True
        , Input.options
            [ ( "Español", Value.custom ES )
            , ( "English", Value.custom EN )
            , ( "Deutsch", Value.custom DE )
            ]
        ]


radioInput : Field id val
radioInput =
    Input.radio
        [ Input.label "Radio inputs"
        , Input.name "radio-inputs"
        , Input.required True
        , Input.hint "Turn the light on or off"
        , Input.options
            [ ( "On", Value.bool True )
            , ( "Off", Value.bool False )
            ]
        ]


groupWithName : Field FieldId val
groupWithName =
    Input.group [ Input.name "group" ] [ stringInput, intInput ]


groupWithNoName : Field FieldId val
groupWithNoName =
    Input.group [] [ stringInput, intInput ]


type BandFields
    = BandMembers
    | BandName
    | MemberName
    | MemberAge


bandFields : Field BandFields val
bandFields =
    Input.group []
        [ Input.text
            [ Input.label "Band Name"
            , Input.required True
            , Input.identifier BandName
            , Input.name "band-name"
            ]
        , Input.group
            [ Input.label "Members (max 5)" ]
            [ Input.repeatable
                [ Input.identifier BandMembers
                , Input.repeatableMin 1
                , Input.repeatableMax 5
                , Input.name "band-members"
                ]
                (Input.group []
                    [ Input.text
                        [ Input.label "Member Name"
                        , Input.identifier MemberName
                        , Input.name "member-name"
                        ]
                    , Input.int
                        [ Input.label "Member Age"
                        , Input.identifier MemberAge
                        , Input.name "member-age"
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


bandDecoder : Decode.Parser BandFields val Band
bandDecoder =
    Decode.map2 Band
        (Decode.field BandName Decode.string)
        (Decode.field BandMembers (Decode.list personDecoder))


personDecoder : Decode.Parser BandFields val Person
personDecoder =
    Decode.map2 Person
        (Decode.field MemberName Decode.string)
        (Decode.field MemberAge Decode.int)
