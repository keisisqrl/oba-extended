module Agency exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Dict


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
