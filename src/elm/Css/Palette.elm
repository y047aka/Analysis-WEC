module Css.Palette exposing (Palette)

import Css.Color exposing (Color, transparent)


type alias Palette =
    { background : Color
    , color : Color
    , border : Color
    , shadow : Color
    }


empty : Palette
empty =
    { background = transparent
    , color = transparent
    , border = transparent
    , shadow = transparent
    }
