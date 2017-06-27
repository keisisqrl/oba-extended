module Utilities exposing (..)

import Json.Decode exposing (string,nullable,Decoder)
import Json.Decode.Pipeline exposing (optional)


maybeString : String -> Decoder (Maybe String -> b) -> Decoder b
maybeString field =
    optional field (nullable string) Nothing
