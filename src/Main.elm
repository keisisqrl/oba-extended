module Main exposing (..)

import Models exposing (..)
import Html
    exposing
        ( Html
        , text
        , li
        , ul
        , button
        , h2
        , div
        , program
        )
import Task
import Html.Events exposing (onClick)
import Geolocation exposing (Location, defaultOptions)
import Http
import Dict
import ObaParser exposing (..)
import ObaRequest exposing (..)


geoOptions : Geolocation.Options
geoOptions =
    { defaultOptions | timeout = Just 60000 }


main =
    program
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        }


initModel : Model
initModel =
    { location = (Location 0 0 0 Nothing Nothing 0)
    , stops = Dict.empty
    , locationError = Nothing
    , httpError = Nothing
    , showStopId = Nothing
    }


init : ( Model, Cmd msg )
init =
    ( initModel, Cmd.none )


type Msg
    = NewLocation (Result Geolocation.Error Location)
    | GetLocation
    | GotStops (Result Http.Error (List Stop))
    | GotLocResponse (Result Http.Error String)
    | ShowStop (Maybe String)
    | Retry


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewLocation loc ->
            case loc of
                Err error ->
                    ( { model | locationError = Just error }, Cmd.none )

                Ok newloc ->
                    { model | location = newloc }
                        |> update Retry

        Retry ->
            ( model, ObaRequest.getStops model.location GotLocResponse )

        GotStops res ->
            case res of
                Err error ->
                    ( { model | httpError = Just error }, Cmd.none )

                Ok stops ->
                    let
                        stopdict =
                            List.map (\n -> ( n.id, n )) stops
                                |> Dict.fromList
                    in
                        ( { model | stops = stopdict }, Cmd.none )

        GotLocResponse res ->
            case res of
                Err error ->
                    ( { model | httpError = Just error }, Cmd.none )
                Ok resp ->
                  case parseLocResponse resp of

                    Ok stops ->
                        ({model | stops = stops}, Cmd.none)

                    Err _ ->
                        (model, Cmd.none)

        GetLocation ->
            ( model
            , Geolocation.nowWith geoOptions
                |> Task.attempt NewLocation
            )

        ShowStop stopId ->
            ({model|showStopId = stopId}, Cmd.none)


subs : Model -> Sub msg
subs _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ case model.showStopId of
            Nothing ->
              stopList model.stops
            Just stopId ->
              showStop (Dict.get stopId model.stops)
        , button
            [ onClick GetLocation ]
            [ text "Update Location" ]
        ]

stopList : StopDict -> Html Msg
stopList stops =
    ul [] (Dict.values (Dict.map stopItem stops))

stopItem : String -> Stop -> Html Msg
stopItem id stop =
  li [ onClick (ShowStop (Just id)) ] [ text stop.name ]

showStop : Maybe Stop -> Html Msg
showStop stop =
  let
    stopName = case stop of
      Just stop ->
        stop.name
      Nothing ->
        "ERROR"

  in
    div [] [ h2 [] [ text stopName ]
    ]
