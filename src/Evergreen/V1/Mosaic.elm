module Evergreen.V1.Mosaic exposing (..)


type alias PictureSize =
    { width : Int
    , height : Int
    }


type alias Picture =
    { id : String
    , size : PictureSize
    }


type Modal
    = PictureOpen Int
    | ModalClosed


type alias DisplayConfig =
    { baseWidth : Int
    , baseHeight : Int
    , spacingSize : Int
    }


type alias Mosaic =
    { pictures : List Picture
    , modal : Modal
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
    | ModalOpen Int
    | ModalExit
    | ReceiveKeyboardEvent KeyBoardKey
