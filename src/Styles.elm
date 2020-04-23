module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (class)



-- COLORS


beige =
    "#fff8f0"


green =
    "#00916e"


blue =
    "#344966"


red =
    "#eb4511"


black =
    "#0a0a0a"


bannerColor =
    green


mainColor =
    beige



--


portionButton : List (Attribute msg)
portionButton =
    [ class "rounded-full"
    , class "w-10"
    , class "h-10"
    , class "items-center"
    ]


imageButton : List (Attribute msg)
imageButton =
    []


detailButton : List (Attribute msg)
detailButton =
    [ class "rounded-full"
    , class "w-10"
    , class "h-10"
    , class "items-center"
    ]
