module Utils.Input exposing (..)

import Element exposing (..)
import Html.Attributes as HA
import Types exposing (..)


letClickThrough : Attribute FrontendMsg
letClickThrough =
    htmlAttribute <|
        HA.style "pointer-events" "none"
