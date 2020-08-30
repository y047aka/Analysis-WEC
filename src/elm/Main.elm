module Main exposing (main)

import AssocList
import AssocList.Extra
import Browser exposing (Document)
import Chart
import Csv
import Csv.Decode as CD exposing (Decoder, Errors(..))
import Data.Car exposing (Car)
import Data.Class as Class exposing (Class(..))
import Data.Lap exposing (Lap, lapDecoder)
import Data.RaceClock as RaceClock
import Html.Styled exposing (Html, main_, tbody, td, text, th, thead, tr)
import Http exposing (Error(..), Expect, Response(..), expectStringResponse)
import List.Extra as List
import Parser exposing (deadEndsToString)


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
            main_ [] [ Chart.lapChart model ]
        ]
    }


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
