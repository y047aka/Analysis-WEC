module Data.Lap exposing (Lap, lapDecoder)

import Csv.Decode as CD exposing (Decoder)
import Data.RaceClock as RaceClock exposing (RaceClock)


type alias Lap =
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


lapDecoder : Decoder (Lap -> a) a
lapDecoder =
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
    CD.map Lap
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
