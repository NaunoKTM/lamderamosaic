module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Element
import Evergreen.V2.Mosaic
import Url


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , message : String
    , maybeDevice : Maybe Element.Device
    , deviceHeight : Int
    , deviceWidth : Int
    , mosaic : Evergreen.V2.Mosaic.Mosaic
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | GotNewSize
        { width : Int
        , height : Int
        }
    | MosaicMsg Evergreen.V2.Mosaic.Msg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
