module TypedSvg.Styled.Attributes.InPx exposing (x, y)

import Svg.Styled exposing (Attribute)
import Svg.Styled.Attributes as Attributes
import TypedSvg.Types exposing (px)
import TypedSvg.TypesToStrings exposing (lengthToString)


x : Float -> Attribute msg
x value =
    Attributes.x <| lengthToString (px value)


y : Float -> Attribute msg
y value =
    Attributes.y <| lengthToString (px value)
