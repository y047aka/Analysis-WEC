module Css.Palette.Svg exposing (SvgPalette, strokeGTEAm, strokeGTEPro, strokeLMP1, strokeLMP2)

import Css.Color exposing (Color, gteAm, gtePro, lmp1, lmp2, transparent)


type alias SvgPalette =
    { fill : Color
    , stroke : Color
    }


empty : SvgPalette
empty =
    { fill = transparent
    , stroke = transparent
    }


strokeLMP1 : SvgPalette
strokeLMP1 =
    { empty | stroke = lmp1 }


strokeLMP2 : SvgPalette
strokeLMP2 =
    { empty | stroke = lmp2 }


strokeGTEPro : SvgPalette
strokeGTEPro =
    { empty | stroke = gtePro }


strokeGTEAm : SvgPalette
strokeGTEAm =
    { empty | stroke = gteAm }
