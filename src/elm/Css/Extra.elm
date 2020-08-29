module Css.Extra exposing (palette, strokeWidth, svgPalette)

import Css exposing (ColorValue, Style, batch, property)
import Css.Color exposing (Color(..))
import Css.Palette exposing (Palette)
import Css.Palette.Svg exposing (SvgPalette)


strokeWidth : Float -> Style
strokeWidth w =
    property "stroke-width" (String.fromFloat w)


{-| Apply colors in batch according to `Palette`

Currently supports `backgroundColor`, `color` and `borderColor`.
It does not set `borderWith` and `borderStyle`, which should be individually set at call sites.

-}
palette : Palette -> Style
palette p =
    batch
        [ backgroundColor p.background
        , color p.color
        , borderColor p.border
        ]


svgPalette : SvgPalette -> Style
svgPalette p =
    batch
        [ fill p.fill
        , stroke p.stroke
        ]


backgroundColor : Color -> Style
backgroundColor c =
    case c of
        ColorValue c_ ->
            Css.backgroundColor c_

        CurrentColor ->
            Css.backgroundColor Css.currentColor

        Transparent ->
            Css.backgroundColor Css.transparent


color : Color -> Style
color c =
    case c of
        ColorValue c_ ->
            Css.color c_

        CurrentColor ->
            Css.color Css.currentColor

        Transparent ->
            Css.color Css.transparent


borderColor : Color -> Style
borderColor c =
    case c of
        ColorValue c_ ->
            Css.borderColor c_

        CurrentColor ->
            Css.borderColor Css.currentColor

        Transparent ->
            Css.borderColor Css.transparent


fill : Color -> Style
fill c =
    case c of
        ColorValue c_ ->
            Css.fill c_

        CurrentColor ->
            Css.fill Css.currentColor

        Transparent ->
            Css.fill Css.transparent


stroke : Color -> Style
stroke c =
    let
        stroke_ =
            .value >> Css.property "stroke"
    in
    case c of
        ColorValue c_ ->
            stroke_ c_

        CurrentColor ->
            stroke_ Css.currentColor

        Transparent ->
            stroke_ Css.transparent
