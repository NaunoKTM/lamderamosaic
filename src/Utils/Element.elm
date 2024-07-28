module Utils.Element exposing (..)

import Element exposing (htmlAttribute)
import Html.Attributes


attributeNone : Element.Attribute msg
attributeNone =
    htmlAttribute <| Html.Attributes.style "none" "none"


edges : { bottom : Int, left : Int, right : Int, top : Int }
edges =
    { bottom = 0, left = 0, right = 0, top = 0 }
