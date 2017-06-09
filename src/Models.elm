module Models exposing (..)

import Http
import Geolocation
import ObaParser exposing (StopDict)

type alias Model =
    { location : Geolocation.Location
    , stops : StopDict
    , locationError : Maybe Geolocation.Error
    , httpError : Maybe Http.Error
    , showStopId : Maybe String
    }
