module ObaParser exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Dict
import Debug


locResponseDecoder : Decoder (List Stop)
locResponseDecoder =
    list stopDecoder
        |> at [ "data", "list" ]


type alias ObaType =
    { id : String }


type alias ObaDict =
    Dict.Dict String ObaType


obaDictFromList : List ObaType -> ObaDict
obaDictFromList input =
    List.map (\n -> ( n.id, n )) input
        |> Dict.fromList


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
        |> optional "direction" (nullable string) Nothing
        |> required "name" string
        |> optional "code" (nullable string) Nothing
        |> required "locationType" int
        |> optional "wheelchairBoarding" (nullable string) Nothing
        |> required "routeIds" (list string)
        |> hardcoded routes

stopDecoder : Decoder Stop
stopDecoder =
  stopDecoderWithRoutes Dict.empty

populateRoutes : Result String RouteDict -> (List Stop) -> (List Stop)
populateRoutes routes stops =
  case routes of
    Err _ ->
      stops
    Ok routeDict ->
      List.map (\stop ->
        {stop | routes = (
          List.map (\n -> (n, (Dict.get n routeDict))) stop.routeIds
          |> Dict.fromList
        )}) stops

type alias Agency =
    { id : String
    , name : String
    , url : String
    , timezone : String
    , lang : Maybe String
    , phone : Maybe String
    , disclaimer : Maybe String
    }


type alias AgencyDict =
    Dict.Dict String Agency


agencyDecoder : Decoder Agency
agencyDecoder =
    decode Agency
        |> required "id" string
        |> required "name" string
        |> required "url" string
        |> required "timezone" string
        |> optional "lang" (nullable string) Nothing
        |> optional "phone" (nullable string) Nothing
        |> optional "disclaimer" (nullable string) Nothing


type RouteType
    = Tram
    | Subway
    | Rail
    | Bus
    | Ferry
    | CableCar
    | Gondola
    | Funicular
    | Error


routeTypeFromInt : Int -> RouteType
routeTypeFromInt value =
    case value of
        0 ->
            Tram

        1 ->
            Subway

        2 ->
            Rail

        3 ->
            Bus

        4 ->
            Ferry

        5 ->
            CableCar

        6 ->
            Gondola

        7 ->
            Funicular

        _ ->
            Error


routeTypeDecoder : Decoder RouteType
routeTypeDecoder =
    map routeTypeFromInt int


type alias Route =
    { id : String
    , shortName : Maybe String
    , longName : Maybe String
    , description : Maybe String
    , route_type : RouteType
    , url : Maybe String
    , color : Maybe String
    , textColor : Maybe String
    , agencyId : String
    , agency : Maybe Agency
    }


type alias RouteDict =
    Dict.Dict String Route


routeDecoder : Decoder Route
routeDecoder =
    decode Route
        |> required "id" string
        |> maybeString "shortName"
        |> maybeString "longName"
        |> maybeString "description"
        |> required "type" routeTypeDecoder
        |> maybeString "url"
        |> maybeString "color"
        |> maybeString "textColor"
        |> required "agencyId" string
        |> hardcoded Nothing

populateAgency : Result String AgencyDict -> (List Route) -> (List Route)
populateAgency agencies routes =
  case agencies of
    Err _ ->
      routes
    Ok agencies ->
      List.map (\route -> {route | agency = (Dict.get route.agencyId agencies)}) routes

maybeString : String -> Decoder (Maybe String -> b) -> Decoder b
maybeString field =
    optional field (nullable string) Nothing
