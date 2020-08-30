module Svg.Styled.Attributes.Typed.InPx exposing (height, width, x, y)

import Svg.Styled exposing (Attribute)
import Svg.Styled.Attributes as Attributes
import TypedSvg.Types exposing (px)
import TypedSvg.TypesToStrings exposing (lengthToString)


height : Float -> Attribute msg
height value =
    Attributes.height <| lengthToString (px value)


width : Float -> Attribute msg
width value =
    Attributes.width <| lengthToString (px value)


x : Float -> Attribute msg
x value =
    Attributes.x <| lengthToString (px value)


y : Float -> Attribute msg
y value =
    Attributes.y <| lengthToString (px value)
