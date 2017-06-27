module Stop exposing (..)

import Dict
import Route exposing (..)
import Utilities exposing (maybeString)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias Stop =
    { id : String
    , lat : Float
    , lon : Float
    , direction : Maybe String
    , name : String
    , code : Maybe String
    , locationType : Int
    , wheelchairBoarding : Maybe String
    , routeIds : List String
    , routes : Dict.Dict String (Maybe Route)
    }


type alias StopDict =
    Dict.Dict String Stop


stopDecoderWithRoutes : Dict.Dict String (Maybe Route) -> Decoder Stop
stopDecoderWithRoutes routes =
    decode Stop
        |> required "id" string
        |> required "lat" float
        |> required "lon" float
        |> maybeString "direction"
        |> required "name" string
        |> maybeString "code"
        |> required "locationType" int
        |> maybeString "wheelchairBoarding"
        |> required "routeIds" (list string)
        |> hardcoded routes


stopDecoder : Decoder Stop
stopDecoder =
    stopDecoderWithRoutes Dict.empty

populateRoutes : RouteDict -> Stop -> Stop
populateRoutes routes stop =
    let
        routes_dict =
            List.map (\n -> (n, Dict.get n routes)) stop.routeIds
             |> Dict.fromList

    in
        {stop | routes = routes_dict}
