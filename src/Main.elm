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
import ObaRequest exposing (..)
import Stop exposing (..)

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


init : ( Model, Cmd msg )
init =
    ( initModel, Cmd.none )


type Msg
    = NewLocation (Result Geolocation.Error Location)
    | GetLocation
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


        GotLocResponse res ->
            case res of
                Err error ->
                    ( { model | httpError = Just error }, Cmd.none )
                Ok resp ->
                  ({model | data = parseLocResponse resp model }, Cmd.none)

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
              stopList model.data.stops
            Just stopId ->
              showStop (Dict.get stopId model.data.stops)
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
