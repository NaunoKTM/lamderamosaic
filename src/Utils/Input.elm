module Utils.Input exposing (..)

import Element exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Lamdera.Json as Json
import Types exposing (..)


greedyOnClick : FrontendMsg -> Attribute FrontendMsg
greedyOnClick msg =
    htmlAttribute <|
        HE.custom "click" (Json.succeed { message = msg, preventDefault = True, stopPropagation = True })


letClickThrough : Attribute FrontendMsg
letClickThrough =
    htmlAttribute <|
        HA.style "pointer-events" "none"
