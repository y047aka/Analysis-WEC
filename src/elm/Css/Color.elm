module Css.Color exposing (Color(..), currentColor, gray, gteAm, gtePro, lmp1, lmp2, transparent)

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


gray : Color
gray =
    ColorValue (Css.hex "#999")


lmp1 : Color
lmp1 =
    ColorValue (Css.hex "#f00")


lmp2 : Color
lmp2 =
    ColorValue (Css.hex "#00f")


gtePro : Color
gtePro =
    ColorValue (Css.hex "#060")


gteAm : Color
gteAm =
    ColorValue (Css.hex "#f60")
