module Support.Shipment exposing
    ( Address
    , CardInformation
    , Recipient
    , Shipment
    )

import Countries exposing (Country)


type alias Shipment =
    { shipping : Address
    , billing : CardInformation
    , recipients : List Recipient
    }


type alias Address =
    { firstName : String
    , lastName : String
    , address : String
    , address2 : String
    , postalCode : String
    , state : String
    , country : Country
    }


type alias CardInformation =
    { cardName : String
    , cardNumber : String
    , cvc : String
    , expireMonth : Int
    , expireYear : Int
    }


type alias Recipient =
    { email : String
    , name : String
    }
