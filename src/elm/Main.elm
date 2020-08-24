module Main exposing (main)

import AssocList
import AssocList.Extra
import Browser exposing (Document)
import Csv
import Csv.Decode as CD exposing (Decoder, Errors(..))
import Data.Lap exposing (Lap, lapDecoder)
import Data.RaceClock as RaceClock
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
    { lapsByCarNumber : List ( Int, List Lap )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { lapsByCarNumber = [] }
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
              }
            , Cmd.none
            )

        Loaded (Err _) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view { lapsByCarNumber } =
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
                        lapsByCarNumber
                ]
            ]
        ]
    }
