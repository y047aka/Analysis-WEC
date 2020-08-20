module Main exposing (main, millisToTimeMilliString, timeMilliStringToMillis)

import AssocList
import AssocList.Extra
import Browser exposing (Document)
import Csv
import Csv.Decode as CD exposing (Decoder, Errors(..))
import Html exposing (main_, table, tbody, td, text, th, thead, tr)
import Http
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
    { lapRecordsByCarNumber : List ( String, List LapRecord )
    }


type alias LapRecord =
    { carNumber : String
    , driverNumber : String
    , lapNumber : String
    , lapTime : String
    , lapImprovement : String
    , crossingFinishLineInPit : String
    , s1 : String
    , s1Improvement : String
    , s2 : String
    , s2Improvement : String
    , s3 : String
    , s3Improvement : String
    , kph : String
    , elapsed : String
    , hour : String
    , topSpeed : String
    , driverName : String
    , pitTime : String
    , class : String
    , group : String
    , team : String
    , manufacturer : String
    }


lapRecordDecoder : Decoder (LapRecord -> a) a
lapRecordDecoder =
    CD.map LapRecord
        (CD.field "NUMBER" Ok
            |> CD.andMap (CD.field "DRIVER_NUMBER" Ok)
            |> CD.andMap (CD.field "LAP_NUMBER" Ok)
            |> CD.andMap (CD.field "LAP_TIME" Ok)
            |> CD.andMap (CD.field "LAP_IMPROVEMENT" Ok)
            |> CD.andMap (CD.field "CROSSING_FINISH_LINE_IN_PIT" Ok)
            |> CD.andMap (CD.field "S1" Ok)
            |> CD.andMap (CD.field "S1_IMPROVEMENT" Ok)
            |> CD.andMap (CD.field "S2" Ok)
            |> CD.andMap (CD.field "S2_IMPROVEMENT" Ok)
            |> CD.andMap (CD.field "S3" Ok)
            |> CD.andMap (CD.field "S3_IMPROVEMENT" Ok)
            |> CD.andMap (CD.field "KPH" Ok)
            |> CD.andMap (CD.field "ELAPSED" Ok)
            |> CD.andMap (CD.field "HOUR" Ok)
            |> CD.andMap (CD.field "TOP_SPEED" Ok)
            |> CD.andMap (CD.field "DRIVER_NAME" Ok)
            |> CD.andMap (CD.field "PIT_TIME" Ok)
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
        , expect = Http.expectString Loaded
        }
    )



-- UPDATE


type Msg
    = Loaded (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loaded (Ok csvString) ->
            ( case decodeString lapRecordDecoder csvString of
                Ok lapRecords ->
                    { model
                        | lapRecordsByCarNumber =
                            lapRecords
                                |> AssocList.Extra.groupBy .carNumber
                                |> AssocList.toList
                    }

                Err _ ->
                    model
            , Cmd.none
            )

        Loaded (Err _) ->
            ( model, Cmd.none )


decodeString : Decoder (a -> a) a -> String -> Result Errors (List a)
decodeString decoder =
    Csv.parseWith ';'
        >> Result.map (\csv -> { csv | headers = List.map String.trim csv.headers })
        >> Result.mapError (deadEndsToString >> List.singleton >> CsvErrors)
        >> Result.andThen (CD.decodeCsv decoder)



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
                                    [ .carNumber, .lapNumber, .lapTime, .s1, .s2, .s3, .kph, .elapsed, .hour, .topSpeed, .driverName, .pitTime, .class, .group, .team, .manufacturer ]
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



-- HELPER


{-|

    timeMilliStringToMillis "0.000"
    --> Just 0

    timeMilliStringToMillis "4.321"
    --> Just 4321

    timeMilliStringToMillis "06:54.321"
    --> Just 414321

    timeMilliStringToMillis "7:06:54.321"
    --> Just 25614321

-}
timeMilliStringToMillis : String -> Maybe Int
timeMilliStringToMillis str =
    let
        fromHours h =
            String.toInt h |> Maybe.map ((*) 3600000)

        fromMinutes m =
            String.toInt m |> Maybe.map ((*) 60000)

        fromSeconds s =
            String.toFloat s |> Maybe.map ((*) 1000 >> floor)
    in
    case String.split ":" str of
        [ h, m, s ] ->
            Maybe.map3 (\h_ m_ s_ -> h_ + m_ + s_)
                (fromHours h)
                (fromMinutes m)
                (fromSeconds s)

        [ m, s ] ->
            Maybe.map2 (+)
                (fromMinutes m)
                (fromSeconds s)

        [ s ] ->
            fromSeconds s

        _ ->
            Nothing


{-|

    millisToTimeMilliString 0
    --> "00:00.000"

    millisToTimeMilliString 4321
    --> "00:04.321"

    millisToTimeMilliString 414321
    --> "06:54.321"

    millisToTimeMilliString 25614321
    --> "07:06:54.321"

-}
millisToTimeMilliString : Int -> String
millisToTimeMilliString millis =
    let
        h =
            (millis // 3600000)
                |> String.fromInt
                |> String.padLeft 2 '0'

        m =
            (remainderBy 3600000 millis // 60000)
                |> String.fromInt
                |> String.padLeft 2 '0'

        s =
            (remainderBy 60000 millis // 1000)
                |> String.fromInt
                |> String.padLeft 2 '0'

        ms =
            remainderBy 1000 millis
                |> String.fromInt
                |> String.padRight 3 '0'
                |> (++) "."
    in
    if millis >= 3600000 then
        String.join ":" [ h, m, s ++ ms ]

    else
        String.join ":" [ m, s ++ ms ]
