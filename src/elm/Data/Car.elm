module Data.Car exposing (Car)

import Data.Class exposing (Class)
import Data.Lap exposing (Lap)


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
