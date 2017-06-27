module Models exposing (..)

import Http
import Geolocation
import Stop exposing (..)
import Route exposing (..)
import Agency exposing (..)
import Dict


type alias Model =
    { location : Geolocation.Location
    , data : ObaData
    , locationError : Maybe Geolocation.Error
    , httpError : Maybe Http.Error
    , showStopId : Maybe String
    }


initModel : Model
initModel =
    { location =
        (Geolocation.Location
            0
            0
            0
            Nothing
            Nothing
            0
        )
    , data = emptyData
    , locationError = Nothing
    , httpError = Nothing
    , showStopId = Nothing
    }


type alias ObaData =
    { stops : StopDict
    , routes : RouteDict
    , agencies : AgencyDict
    }


emptyData : ObaData
emptyData =
    { stops = Dict.empty
    , routes = Dict.empty
    , agencies = Dict.empty
    }
