module Css.Color exposing (Color(..), currentColor, transparent)

import Css


type Color
    = ColorValue Css.Color
    | CurrentColor
    | Transparent


currentColor : Color
currentColor =
    CurrentColor


transparent : Color
transparent =
    Transparent
