module Main exposing (main)

import AssocList
import AssocList.Extra
import Browser exposing (Document)
import Csv
import Csv.Decode as CD exposing (Decoder, Errors(..))
import Data.RaceClock as RaceClock exposing (RaceClock)
import Html exposing (main_, table, tbody, td, text, th, thead, tr)
import Http exposing (Error(..), Expect, Response(..), expectStringResponse)
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
    { lapRecordsByCarNumber : List ( Int, List LapRecord )
    }


type alias LapRecord =
    { carNumber : Int
    , driverNumber : Int
    , lapNumber : Int
    , lapTime : RaceClock
    , lapImprovement : Int
    , crossingFinishLineInPit : String
    , s1 : RaceClock
    , s1Improvement : Int
    , s2 : RaceClock
    , s2Improvement : Int
    , s3 : RaceClock
    , s3Improvement : Int
    , kph : Float
    , elapsed : RaceClock
    , hour : RaceClock
    , topSpeed : Float
    , driverName : String
    , pitTime : Maybe RaceClock
    , class : String
    , group : String
    , team : String
    , manufacturer : String
    }


lapRecordDecoder : Decoder (LapRecord -> a) a
lapRecordDecoder =
    let
        stringToIntResult : String -> Result String Int
        stringToIntResult s =
            String.toInt s
                |> Result.fromMaybe ("Cannot convert '" ++ s ++ "' to Int")

        stringToFloatResult : String -> Result String Float
        stringToFloatResult s =
            String.toFloat s
                |> Result.fromMaybe ("Cannot convert '" ++ s ++ "' to Float")

        stringToRaceClockResult : String -> Result String Int
        stringToRaceClockResult s =
            RaceClock.fromString s
                |> Result.fromMaybe ("Cannot convert '" ++ s ++ "' to Int")
    in
    CD.map LapRecord
        (CD.field "NUMBER" stringToIntResult
            |> CD.andMap (CD.field "DRIVER_NUMBER" stringToIntResult)
            |> CD.andMap (CD.field "LAP_NUMBER" stringToIntResult)
            |> CD.andMap (CD.field "LAP_TIME" stringToRaceClockResult)
            |> CD.andMap (CD.field "LAP_IMPROVEMENT" stringToIntResult)
            |> CD.andMap (CD.field "CROSSING_FINISH_LINE_IN_PIT" Ok)
            |> CD.andMap (CD.field "S1" stringToRaceClockResult)
            |> CD.andMap (CD.field "S1_IMPROVEMENT" stringToIntResult)
            |> CD.andMap (CD.field "S2" stringToRaceClockResult)
            |> CD.andMap (CD.field "S2_IMPROVEMENT" stringToIntResult)
            |> CD.andMap (CD.field "S3" stringToRaceClockResult)
            |> CD.andMap (CD.field "S3_IMPROVEMENT" stringToIntResult)
            |> CD.andMap (CD.field "KPH" stringToFloatResult)
            |> CD.andMap (CD.field "ELAPSED" stringToRaceClockResult)
            |> CD.andMap (CD.field "HOUR" stringToRaceClockResult)
            |> CD.andMap (CD.field "TOP_SPEED" stringToFloatResult)
            |> CD.andMap (CD.field "DRIVER_NAME" Ok)
            |> CD.andMap (CD.field "PIT_TIME" <| CD.maybe stringToRaceClockResult)
            |> CD.andMap (CD.field "CLASS" Ok)
            |> CD.andMap (CD.field "GROUP" Ok)
            |> CD.andMap (CD.field "TEAM" Ok)
            |> CD.andMap (CD.field "MANUFACTURER" Ok)
        )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { lapRecordsByCarNumber = [] }
    , Http.get
        { url = "23_Analysis_Race_Hour 6.csv"
        , expect = expectCsv Loaded lapRecordDecoder
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
    = Loaded (Result Http.Error (List LapRecord))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loaded (Ok lapRecords) ->
            ( { model
                | lapRecordsByCarNumber =
                    lapRecords
                        |> AssocList.Extra.groupBy .carNumber
                        |> AssocList.toList
              }
            , Cmd.none
            )

        Loaded (Err _) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view { lapRecordsByCarNumber } =
    { title = ""
    , body =
        [ main_ []
            [ table []
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
                        lapRecordsByCarNumber
                ]
            ]
        ]
    }
