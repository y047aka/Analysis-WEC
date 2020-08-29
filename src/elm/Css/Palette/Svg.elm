module Css.Palette.Svg exposing (SvgPalette, strokeAxis, strokeGTEAm, strokeGTEPro, strokeLMP1, strokeLMP2, textOptional)

import Css.Color exposing (Color, gray, gteAm, gtePro, lmp1, lmp2, transparent)


type alias SvgPalette =
    { fill : Color
    , stroke : Color
    }


empty : SvgPalette
empty =
    { fill = transparent
    , stroke = transparent
    }


textOptional : SvgPalette
textOptional =
    { empty | fill = gray }


strokeAxis : SvgPalette
strokeAxis =
    { empty | stroke = gray }


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
