module Route exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Utilities exposing (maybeString)
import Dict
import Agency exposing (..)


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


populateAgency : AgencyDict -> Route -> Route
populateAgency agencies route =
    { route | agency = Dict.get route.agencyId agencies }
