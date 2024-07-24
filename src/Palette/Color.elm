module Palette.Color exposing (..)

import Element exposing (Color, rgb255, rgba)


{-| #000000
-}
black : Color
black =
    rgb255 0 0 0


blackNonOpaque : Color
blackNonOpaque =
    rgba 0 0 0 0.4


blackLittleOpaque : Color
blackLittleOpaque =
    rgba 0 0 0 0.7


{-| #242525
-}
g100 : Color
g100 =
    rgb255 24 25 25


{-| #4f5352
-}
g200 : Color
g200 =
    rgb255 79 83 82


{-| #d2d2d2
-}
g300 : Color
g300 =
    rgb255 100 100 100


{-| #333333
-}
g390 : Color
g390 =
    rgb255 85 85 85


{-| #b2b2b2
-}
g400 : Color
g400 =
    rgb255 218 212 203


g500 : Color
g500 =
    rgb255 183 183 183


{-| #3C3C3C
-}
g600 : Color
g600 =
    rgb255 60 60 60


{-| #333333
-}
g700 : Color
g700 =
    rgb255 110 110 110


{-| #00c853
-}
green : Color
green =
    rgb255 0 200 83


{-| #00bcd4
-}
lightBlue : Color
lightBlue =
    rgb255 0 188 212


{-| #6493c8
-}
lightBlue2 : Color
lightBlue2 =
    rgb255 100 147 200


{-| #ff6e40
-}
orange : Color
orange =
    rgb255 255 110 64


{-| #9C241B
-}
red : Color
red =
    rgb255 156 36 27


{-| #ffffff
-}
white : Color
white =
    rgb255 255 255 255


whiteNonOpaque : Color
whiteNonOpaque =
    rgba 255 255 255 0.3


white8Opaque : Color
white8Opaque =
    rgba 255 255 255 0.8


white9Opaque : Color
white9Opaque =
    rgba 255 255 255 0.9


transparentxx : Color
transparentxx =
    rgba 0 0 0 0.0


greylogo : Color
greylogo =
    rgb255 84 84 84


brokewhite : Color
brokewhite =
    rgb255 245 245 220


backcolor : Color
backcolor =
    rgb255 201 201 201


lightgrey : Color
lightgrey =
    rgb255 169 169 169


whitenoshine : Color
whitenoshine =
    rgb255 235 229 299


gradientbg : List Color
gradientbg =
    [ whitenoshine
    , lightgrey
    , backcolor
    ]


gradientWarm : List Color
gradientWarm =
    [ rgb255 255 165 0 -- Orange
    , rgb255 255 99 71 -- Tomato
    , rgb255 255 69 0 -- Red-Orange
    ]


gradientEarth : List Color
gradientEarth =
    [ rgb255 139 69 19 -- Saddle Brown
    , rgb255 160 82 45 -- Sienna
    , rgb255 210 180 140 -- Tan
    ]


gradientForest : List Color
gradientForest =
    [ rgb255 34 139 34 -- Forest Green
    , rgb255 0 128 0 -- Green
    , rgb255 144 238 144 -- Light Green
    ]


gradientWhiteGrey : List Color
gradientWhiteGrey =
    [ rgb255 255 255 255 -- White
    , rgb255 230 230 230 -- Very Light Grey
    , rgb255 200 200 200 -- Light Grey
    ]
