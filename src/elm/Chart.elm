module Chart exposing (lapChart)

import Css exposing (block, display)
import Css.Extra exposing (strokeWidth, svgPalette)
import Css.Global exposing (descendants, each)
import Css.Palette.Svg exposing (..)
import Data.Car exposing (Car)
import Data.Class exposing (Class(..))
import Html.Styled exposing (text)
import List.Extra as List
import Scale
import Svg.Styled exposing (Svg, g, polyline, svg)
import Svg.Styled.Attributes as Svg
import Svg.Styled.Attributes.Typed exposing (points, transform, viewBox)
import Svg.Styled.Attributes.Typed.InPx exposing (height, width, x, y)
import Svg.Styled.Axis as Axis
import TypedSvg.Types exposing (Paint(..), Transform(..))



-- VIEW


w : Float
w =
    1600


h : Float
h =
    w * (9 / 16)


padding : { top : Float, left : Float, bottom : Float, right : Float }
padding =
    { top = 25 + 30, left = 20 + 190, bottom = 25 + 30, right = 20 }


lapChart : { cars : List Car, ordersByLap : List { lapNumber : Int, order : List Int } } -> Svg msg
lapChart { cars, ordersByLap } =
    let
        xScale =
            List.length ordersByLap
                |> (\max -> ( 0, toFloat max ))
                |> Scale.linear ( padding.left, w - padding.right )

        yScale =
            (List.length cars - 1)
                |> (\max -> ( 0, toFloat max ))
                |> Scale.linear ( padding.top, h - padding.bottom )

        xAxis =
            g
                [ Svg.css
                    [ descendants
                        [ Css.Global.typeSelector "text" [ svgPalette textOptional ]
                        , each
                            [ Css.Global.typeSelector "line"
                            , Css.Global.typeSelector "path"
                            ]
                            [ strokeWidth 1
                            , svgPalette strokeAxis
                            ]
                        ]
                    ]
                ]
                [ g [ transform [ Translate 0 (padding.top - 20) ] ]
                    [ Axis.top
                        [ Axis.tickCount <| (List.length ordersByLap // 10)
                        , Axis.tickSizeOuter 5
                        , Axis.tickSizeInner 5
                        ]
                        xScale
                    ]
                , g [ transform [ Translate 0 (h - padding.bottom + 20) ] ]
                    [ Axis.bottom
                        [ Axis.tickCount <| (List.length ordersByLap // 10)
                        , Axis.tickSizeOuter 5
                        , Axis.tickSizeInner 5
                        ]
                        xScale
                    ]
                ]

        historyFor car =
            g []
                [ heading car

                -- , positionsGroup car
                , positionsPolyline car
                ]

        heading { carNumber, team, startPosition } =
            let
                coordinate =
                    { x = Scale.convert xScale -20
                    , y = (+) 5 <| Scale.convert yScale <| toFloat <| startPosition
                    }

                label =
                    String.join " " [ String.fromInt carNumber, team ]
            in
            g [] [ Svg.Styled.text_ [ x coordinate.x, y coordinate.y ] [ text label ] ]

        positionsGroup { carNumber, startPosition, positions } =
            g [] <|
                List.indexedMap
                    (\index position ->
                        let
                            coordinate =
                                { x = Scale.convert xScale <| toFloat <| index
                                , y = Scale.convert yScale <| toFloat <| position
                                }

                            label =
                                String.fromInt carNumber
                        in
                        Svg.Styled.text_ [ x coordinate.x, y coordinate.y ] [ text label ]
                    )
                    (startPosition :: positions)

        positionsPolyline { class, startPosition, positions } =
            let
                svgPalette_ =
                    case class of
                        LMP1 ->
                            strokeLMP1

                        LMP2 ->
                            strokeLMP2

                        LMGTE_Pro ->
                            strokeGTEPro

                        LMGTE_Am ->
                            strokeGTEAm
            in
            polyline
                [ Svg.css [ svgPalette svgPalette_ ]
                , points <|
                    List.indexedMap
                        (\index position ->
                            let
                                coordinate =
                                    { x = Scale.convert xScale <| toFloat <| index
                                    , y = Scale.convert yScale <| toFloat <| position
                                    }
                            in
                            ( coordinate.x, coordinate.y )
                        )
                        (startPosition :: positions)
                ]
                []
    in
    svg
        [ width w
        , height h
        , viewBox 0 0 w h
        , Svg.css [ display block ]
        ]
        [ g []
            (cars
                |> List.sortBy .startPosition
                |> List.map historyFor
            )
        , xAxis
        ]
