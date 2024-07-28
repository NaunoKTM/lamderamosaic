module Evergreen.V1.Mosaic exposing (..)


type Modal
    = PictureOpen Int


type alias PictureSize =
    { width : Int
    , height : Int
    }


type alias Picture =
    { id : String
    , size : PictureSize
    }


type alias DisplayConfig =
    { baseWidth : Int
    , baseHeight : Int
    , spacingSize : Int
    }


type alias Mosaic =
    { pictures : List Picture
    , modal : Maybe Modal
    , config : DisplayConfig
    , screenSize :
        { deviceWidth : Int
        , deviceHeight : Int
        }
    }


type KeyBoardKey
    = Left
    | Right
    | Escape
    | Other


type Msg
    = NoOpMsg
    | ModalOpen Modal
    | ModalExit
    | ReceiveKeyboardEvent KeyBoardKey
