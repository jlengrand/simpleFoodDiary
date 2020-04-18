port module Ports exposing (..)

import Json.Encode


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port saveFoodLog : Json.Encode.Value -> Cmd msg


port signInInfo : (Json.Encode.Value -> msg) -> Sub msg
