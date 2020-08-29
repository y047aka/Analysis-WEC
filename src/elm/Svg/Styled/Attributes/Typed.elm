module Svg.Styled.Attributes.Typed exposing (fill, points, stroke, transform, viewBox)

import Svg.Styled exposing (Attribute)
import Svg.Styled.Attributes as Attributes
import TypedSvg.Types exposing (Paint, Transform)
import TypedSvg.TypesToStrings exposing (paintToString, transformToString)


fill : Paint -> Attribute msg
fill =
    Attributes.fill << paintToString


points : List ( Float, Float ) -> Attribute msg
points pts =
    let
        pointToString ( xx, yy ) =
            String.fromFloat xx ++ ", " ++ String.fromFloat yy
    in
    Attributes.points <| String.join " " (List.map pointToString pts)


stroke : Paint -> Attribute msg
stroke =
    Attributes.stroke << paintToString


transform : List Transform -> Attribute msg
transform transforms =
    Attributes.transform <| String.join " " (List.map transformToString transforms)


viewBox : Float -> Float -> Float -> Float -> Attribute a
viewBox minX minY vWidth vHeight =
    [ minX, minY, vWidth, vHeight ]
        |> List.map String.fromFloat
        |> String.join " "
        |> Attributes.viewBox
