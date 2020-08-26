module Main exposing (main)

import AssocList
import AssocList.Extra
import Browser exposing (Document)
import Css.Extra exposing (noStyle, svgPalette)
import Css.Palette.Svg exposing (strokeGTEAm, strokeGTEPro, strokeLMP1, strokeLMP2)
import Csv
import Csv.Decode as CD exposing (Decoder, Errors(..))
import Data.Lap exposing (Lap, lapDecoder)
import Data.RaceClock as RaceClock
import Html.Styled exposing (Html, main_, tbody, td, text, th, thead, tr)
import Http exposing (Error(..), Expect, Response(..), expectStringResponse)
import List.Extra as List
import Parser exposing (deadEndsToString)
import Svg.Styled exposing (Svg, g, polyline, svg)
import Svg.Styled.Attributes as Svg
import Svg.Styled.Attributes.Typed exposing (points, viewBox)
import Svg.Styled.Attributes.Typed.InPx exposing (x, y)
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
    { lapsByCarNumber : List ( Int, List Lap )
    , ordersByLap : OrdersByLap
    }


type alias OrdersByLap =
    List { lapNumber : Int, order : List Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { lapsByCarNumber = [], ordersByLap = [] }
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
            ( { model
                | lapsByCarNumber =
                    laps
                        |> AssocList.Extra.groupBy .carNumber
                        |> AssocList.toList
                , ordersByLap =
                    laps
                        |> AssocList.Extra.groupBy .lapNumber
                        |> AssocList.toList
                        |> List.map
                            (\( lapNumber, order ) ->
                                { lapNumber = lapNumber
                                , order = order |> List.sortBy .elapsed |> List.map .carNumber
                                }
                            )
              }
            , Cmd.none
            )

        Loaded (Err _) ->
            ( model, Cmd.none )


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
    1700


h : Float
h =
    900


lapChart : Model -> Svg msg
lapChart m =
    let
        historyFor startPosition ( _, laps ) =
            g []
                [ heading startPosition laps

                -- , g [] <| positions laps
                , positionsPolyline laps
                ]

        heading startPosition laps =
            List.head laps
                |> Maybe.map
                    (\l ->
                        g []
                            [ Svg.Styled.text_
                                [ x 10
                                , y <| toFloat <| (+) 30 <| (*) 30 <| startPosition
                                ]
                                [ text <| String.join " " [ String.fromInt l.carNumber, l.team ] ]
                            ]
                    )
                |> Maybe.withDefault (text "")

        positions laps =
            List.map
                (\{ carNumber, lapNumber } ->
                    let
                        currentPosition =
                            getPositionAt { carNumber = carNumber, lapNumber = lapNumber } m.ordersByLap
                                |> Maybe.withDefault 0
                    in
                    Svg.Styled.text_
                        [ x <| toFloat <| (+) 200 <| (*) 10 <| lapNumber
                        , y <| toFloat <| (+) 30 <| (*) 30 <| currentPosition
                        ]
                        [ text (String.fromInt carNumber) ]
                )
                laps

        positionsPolyline laps =
            polyline
                [ Svg.css
                    [ case List.head laps |> Maybe.map .class |> Maybe.withDefault "" of
                        "LMP1" ->
                            svgPalette strokeLMP1

                        "LMP2" ->
                            svgPalette strokeLMP2

                        "LMGTE Pro" ->
                            svgPalette strokeGTEPro

                        "LMGTE Am" ->
                            svgPalette strokeGTEAm

                        _ ->
                            noStyle
                    ]
                , points <|
                    List.map
                        (\{ carNumber, lapNumber } ->
                            let
                                currentPosition =
                                    getPositionAt { carNumber = carNumber, lapNumber = lapNumber } m.ordersByLap
                                        |> Maybe.withDefault 0
                            in
                            ( toFloat <| (+) 205 <| (*) 10 <| lapNumber
                            , toFloat <| (+) 25 <| (*) 30 <| currentPosition
                            )
                        )
                        laps
                ]
                []
    in
    svg [ viewBox 0 0 w h ] <|
        (m.lapsByCarNumber
            |> List.sortBy (Tuple.second >> List.head >> Maybe.map .elapsed >> Maybe.withDefault 0)
            |> List.indexedMap historyFor
        )


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
                            , .class
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
