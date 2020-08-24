module Data.RaceClock exposing (RaceClock, fromString, toString)


type alias RaceClock =
    Int


{-|

    fromString "0.000"
    --> Just 0

    fromString "4.321"
    --> Just 4321

    fromString "06:54.321"
    --> Just 414321

    fromString "7:06:54.321"
    --> Just 25614321

-}
fromString : String -> Maybe RaceClock
fromString str =
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

    toString 0
    --> "00:00.000"

    toString 4321
    --> "00:04.321"

    toString 414321
    --> "06:54.321"

    toString 25614321
    --> "07:06:54.321"

-}
toString : RaceClock -> String
toString millis =
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
