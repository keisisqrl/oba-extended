module ObaRequest exposing (getStops,parseLocResponse)

import Http
import Geolocation exposing (Location)
import ObaParser exposing (..)
import Dict
import Json.Decode as Decode
import Debug


corsProxy : String
corsProxy =
    "https://www.crossorigin.me/"


apiPrefix : String
apiPrefix =
    "http://api.pugetsound.onebusaway.org/"
        ++ "api/where/stops-for-location.json"
        ++ "?key=9957810a-bee2-42c3-825f-08d5691fe70f"


getStops :
    Location
    -> (Result Http.Error String -> msg)
    -> Cmd msg
getStops loc msg =
    let
        latString =
            "&lat=" ++ toString loc.latitude

        lonString =
            "&lon=" ++ toString loc.longitude

        reqUrl =
            corsProxy
                ++ apiPrefix
                ++ latString
                ++ lonString
    in
        Http.getString reqUrl
            |> Http.send msg

parseLocResponse : String -> Result String StopDict
parseLocResponse resp =
  let
    agencies = (Decode.list agencyDecoder
               |> Decode.at ["data","references","agencies"]
               |> Decode.decodeString) resp
               |> resultListToDict
    routes = (Decode.list routeDecoder
               |> Decode.at ["data","references","routes"]
               |> Decode.decodeString) resp
               |> Result.map (populateAgency agencies)
               |> resultListToDict

  in
    Decode.decodeString (Decode.at ["data","list"] (Decode.list stopDecoder)) resp
    |> Result.map (populateRoutes routes)
    |> resultListToDict

resultListToDict
  : Result x (List { a | id : comparable })
  -> Result x (Dict.Dict comparable { a | id : comparable })
resultListToDict reslist =
  reslist
  |> Result.map (List.map (\n -> (n.id, n)))
  |> Result.map Dict.fromList
