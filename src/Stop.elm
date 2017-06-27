module Stop exposing (..)

import Dict
import Route exposing (..)
import Utilities exposing (maybeString)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)


--- Type definitions


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



--- Processing functions


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
            List.map (\n -> ( n, Dict.get n routes )) stop.routeIds
                |> Dict.fromList
    in
        { stop | routes = routes_dict }



--- Rendering


stopList : StopDict -> Html Msg
stopList stops =
    ul [] (Dict.values (Dict.map stopItem stops))


stopItem : String -> Stop -> Html Msg
stopItem id stop =
    li [ onClick (ShowStop (Just id)) ] [ text stop.name ]


showStop : Maybe Stop -> Html Msg
showStop isStop =
    case isStop of
        Nothing ->
            div [] [ h2 [] [ text "ERROR" ] ]

        Just stop ->
            div []
                [ h2 [ onClick (ShowStop Nothing) ] [ text stop.name ]
                , maybeRouteList stop.routes
                ]


maybeRouteList : Dict.Dict String (Maybe Route) -> Html Msg
maybeRouteList routes =
    ul []
        (routes
            |> Dict.map
                (\x y ->
                    case y of
                        Just route ->
                            routeItem route.id route

                        Nothing ->
                            text ""
                )
            |> Dict.values
        )
