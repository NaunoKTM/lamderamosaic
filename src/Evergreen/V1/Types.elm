module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Element
import Http
import Url


type Modal
    = PictureOpen Int Int


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , message : String
    , maybeDevice : Maybe Element.Device
    , modal : Maybe Modal
    , deviceHeight : Int
    , deviceWidth : Int
    }


type alias BackendModel =
    { message : String
    }


type KeyBoardKey
    = Left
    | Right
    | Escape
    | Other


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | GotNewSize
        { width : Int
        , height : Int
        }
    | ReceiveWindowSize (Result Http.Error Browser.Dom.Viewport)
    | ReceiveKeyboardEvent KeyBoardKey
    | ModalOpen Modal
    | ModalExit


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
