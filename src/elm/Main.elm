module Main exposing (main)

import AssocList
import AssocList.Extra
import Browser exposing (Document)
import Css exposing (block, display)
import Css.Extra exposing (strokeWidth, svgPalette)
import Css.Global exposing (descendants, each)
import Css.Palette.Svg exposing (..)
import Csv
import Csv.Decode as CD exposing (Decoder, Errors(..))
import Data.Class as Class exposing (Class(..))
import Data.Lap exposing (Lap, lapDecoder)
import Data.RaceClock as RaceClock
import Html.Styled exposing (Html, main_, tbody, td, text, th, thead, tr)
import Http exposing (Error(..), Expect, Response(..), expectStringResponse)
import List.Extra as List
import Parser exposing (deadEndsToString)
import Scale
import Svg.Styled exposing (Svg, g, polyline, svg)
import Svg.Styled.Attributes as Svg
import Svg.Styled.Attributes.Typed exposing (points, transform, viewBox)
import Svg.Styled.Attributes.Typed.InPx exposing (x, y)
import Svg.Styled.Axis as Axis
import TypedSvg.Types exposing (Paint(..), Transform(..))


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { cars : List Car
    , ordersByLap : OrdersByLap
    }


type alias OrdersByLap =
    List { lapNumber : Int, order : List Int }


type alias Car =
    { carNumber : Int
    , class : Class
    , group : String
    , team : String
    , manufacturer : String
    , startPosition : Int
    , positions : List Int
    , laps : List Lap
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cars = [], ordersByLap = [] }
    , Http.get
        { url = "23_Analysis_Race_Hour 6.csv"
        , expect = expectCsv Loaded lapDecoder
        }
    )


expectCsv : (Result Error (List a) -> msg) -> Decoder (a -> a) a -> Expect msg
expectCsv toMsg decoder =
    let
        resolve : (body -> Result String (List a)) -> Response body -> Result Error (List a)
        resolve toResult response =
            case response of
                BadUrl_ url ->
                    Err (BadUrl url)

                Timeout_ ->
                    Err Timeout

                NetworkError_ ->
                    Err NetworkError

                BadStatus_ metadata _ ->
                    Err (BadStatus metadata.statusCode)

                GoodStatus_ _ body ->
                    Result.mapError BadBody (toResult body)

        errorsToString : Errors -> String
        errorsToString error =
            case error of
                CsvErrors _ ->
                    "Parse failed."

                DecodeErrors _ ->
                    "Decode failed."
    in
    expectStringResponse toMsg <|
        resolve
            (Csv.parseWith ';'
                >> Result.map (\csv -> { csv | headers = List.map String.trim csv.headers })
                >> Result.mapError (deadEndsToString >> List.singleton >> CsvErrors)
                >> Result.andThen (CD.decodeCsv decoder)
                >> Result.mapError errorsToString
            )



-- UPDATE


type Msg
    = Loaded (Result Http.Error (List Lap))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loaded (Ok laps) ->
            let
                ordersByLap =
                    laps
                        |> AssocList.Extra.groupBy .lapNumber
                        |> AssocList.toList
                        |> List.map
                            (\( lapNumber, order ) ->
                                { lapNumber = lapNumber
                                , order = order |> List.sortBy .elapsed |> List.map .carNumber
                                }
                            )

                cars =
                    laps
                        |> AssocList.Extra.groupBy .carNumber
                        |> AssocList.toList
                        |> List.filterMap (summarize ordersByLap)
            in
            ( { model | cars = cars, ordersByLap = ordersByLap }, Cmd.none )

        Loaded (Err _) ->
            ( model, Cmd.none )


summarize : OrdersByLap -> ( Int, List Lap ) -> Maybe Car
summarize ordersByLap ( carNumber, laps ) =
    List.head laps
        |> Maybe.map
            (\{ class, group, team, manufacturer } ->
                { carNumber = carNumber
                , class = class
                , group = group
                , team = team
                , manufacturer = manufacturer
                , startPosition = Maybe.withDefault 0 <| getPositionAt { carNumber = carNumber, lapNumber = 1 } ordersByLap
                , positions =
                    List.indexedMap
                        (\index _ -> Maybe.withDefault 0 <| getPositionAt { carNumber = carNumber, lapNumber = index + 1 } ordersByLap)
                        laps
                , laps = laps
                }
            )


getPositionAt : { carNumber : Int, lapNumber : Int } -> OrdersByLap -> Maybe Int
getPositionAt { carNumber, lapNumber } ordersByLap =
    ordersByLap
        |> List.find (.lapNumber >> (==) lapNumber)
        |> Maybe.andThen (.order >> List.findIndex ((==) carNumber))



-- VIEW


view : Model -> Document Msg
view model =
    { title = ""
    , body =
        [ Html.Styled.toUnstyled <|
            main_ [] [ lapChart model ]
        ]
    }


w : Float
w =
    1600


h : Float
h =
    w * (9 / 16)


padding : { top : Float, left : Float, bottom : Float, right : Float }
padding =
    { top = 25 + 30, left = 20 + 190, bottom = 25 + 30, right = 20 }


lapChart : Model -> Svg msg
lapChart m =
    let
        xScale =
            List.length m.ordersByLap
                |> (\max -> ( 0, toFloat max ))
                |> Scale.linear ( padding.left, w - padding.right )

        yScale =
            (List.length m.cars - 1)
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
                        [ Axis.tickCount <| (List.length m.ordersByLap // 10)
                        , Axis.tickSizeOuter 5
                        , Axis.tickSizeInner 5
                        ]
                        xScale
                    ]
                , g [ transform [ Translate 0 (h - padding.bottom + 20) ] ]
                    [ Axis.bottom
                        [ Axis.tickCount <| (List.length m.ordersByLap // 10)
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
        [ viewBox 0 0 w h
        , Svg.css [ display block ]
        ]
        [ g []
            (m.cars
                |> List.sortBy .startPosition
                |> List.map historyFor
            )
        , xAxis
        ]


decodeTestTable : List ( Int, List Lap ) -> Html msg
decodeTestTable lapsByCarNumber =
    Html.Styled.table []
        [ thead []
            [ tr [] <|
                List.map (\heading -> th [] [ text heading ])
                    [ "NUMBER", "LAP_NUMBER", "LAP_TIME", "S1", "S2", "S3", "KPH", "ELAPSED", "HOUR", "TOP_SPEED", "DRIVER_NAME", "PIT_TIME", "CLASS", "GROUP", "TEAM", "MANUFACTURER" ]
            ]
        , tbody [] <|
            let
                tableRow lap =
                    tr [] <|
                        List.map (\getter -> td [] [ text <| getter lap ])
                            [ .carNumber >> String.fromInt
                            , .lapNumber >> String.fromInt
                            , .lapTime >> RaceClock.toString
                            , .s1 >> RaceClock.toString
                            , .s2 >> RaceClock.toString
                            , .s3 >> RaceClock.toString
                            , .kph >> String.fromFloat
                            , .elapsed >> RaceClock.toString
                            , .hour >> RaceClock.toString
                            , .topSpeed >> String.fromFloat
                            , .driverName
                            , .pitTime >> Maybe.map RaceClock.toString >> Maybe.withDefault ""
                            , .class >> Class.toString
                            , .group
                            , .team
                            , .manufacturer
                            ]
            in
            List.map
                (Tuple.second
                    >> List.head
                    >> Maybe.map tableRow
                    >> Maybe.withDefault (text "")
                )
                lapsByCarNumber
        ]
