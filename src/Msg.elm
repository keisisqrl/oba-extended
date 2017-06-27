module Msg exposing (..)

import Http
import Geolocation


type Msg
    = NewLocation (Result Geolocation.Error Geolocation.Location)
    | GetLocation
    | GotLocResponse (Result Http.Error String)
    | ShowStop (Maybe String)
    | Retry
