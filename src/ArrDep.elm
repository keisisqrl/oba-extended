module ArrDep exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Utilities exposing (maybeString)
import Dict
import Route exposing (..)
import Html exposing (..)
import Msg exposing (..)
import Stop exposing (..)
import Route exposing (..)


--- Type definitions


type alias ArrDep =
    { routeId : String
    , route : Maybe Route
    , tripId : String
    , serviceDate : Int
    , stopId : String
    , stop : Maybe Stop
    , stopSequence : String
    , blockTripSequence : String
    , tripHeadsign : Maybe String
    , scheduledArrivalTime : Int
    , predicted : Bool
    , predictedArrivalTime : Int
    }


type alias ArrDepDict =
    Dict.Dict String ArrDep



--- Processing functions


arrDepDecoder : Decoder ArrDep
arrDepDecoder =
    decode ArrDep
        |> required "routeId" string
        |> hardcoded Nothing
        |> required "tripId" string
        |> required "serviceDate" int
        |> required "stopId" string
        |> hardcoded Nothing
        |> required "stopSequence" string
        |> required "blockTripSequence" string
        |> maybeString "tripHeadsign"
        |> required "scheduledArrivalTime" int
        |> required "predicted" bool
        |> required "predictedArrivalTime" int





--- Rendering functions


routeList : RouteDict -> Html Msg
routeList routes =
    ul [] (Dict.values (Dict.map routeItem routes))


routeItem : String -> Route -> Html Msg
routeItem id route =
    let
        routeName =
            Maybe.withDefault "" route.shortName

        routeDesc =
            Maybe.withDefault "" route.description

        agencyName =
            case route.agency of
                Nothing ->
                    ""

                Just agency ->
                    agency.name
    in
        li []
            [ strong []
                [ text agencyName
                , text " "
                , text routeName
                ]
            , text ": "
            , text routeDesc
            ]
