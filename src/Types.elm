module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation exposing (Key)
import Element exposing (Device)
import Http
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , maybeDevice : Maybe Device
    , modal : Maybe Modal
    , deviceHeight : Int
    , deviceWidth : Int
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GotNewSize { width : Int, height : Int }
    | ReceiveWindowSize (Result Http.Error Dom.Viewport)
    | ReceiveKeyboardEvent KeyBoardKey
    | ModalOpen Modal
    | ModalExit


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend


type alias Picture =
    { id : String
    , size : ImageSize
    }


type alias ImageSize =
    { width : Int
    , height : Int
    }


type Modal
    = PictureOpen Int Int


type KeyBoardKey
    = Left
    | Right
    | Escape
    | Other


type alias DisplayConfig =
    { baseWidth : Int
    , baseHeight : Int
    , spacingSize : Int
    }
