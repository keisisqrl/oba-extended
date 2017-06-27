module ObaParser exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Dict
import Debug
import Route exposing (..)
import Stop exposing (..)
import Agency exposing (..)


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



