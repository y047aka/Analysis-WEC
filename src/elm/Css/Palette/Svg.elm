module Css.Palette.Svg exposing (SvgPalette)

import Css.Color exposing (Color, transparent)


type alias SvgPalette =
    { fill : Color
    , stroke : Color
    }


empty : SvgPalette
empty =
    { fill = transparent
    , stroke = transparent
    }
