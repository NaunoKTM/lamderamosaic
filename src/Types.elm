module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Element exposing (Device)
import Mosaic exposing (Modal, Mosaic)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , maybeDevice : Maybe Device
    , modal : Maybe Modal
    , deviceHeight : Int
    , deviceWidth : Int
    , mosaic : Mosaic
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GotNewSize { width : Int, height : Int }
    | MosaicMsg Mosaic.Msg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
