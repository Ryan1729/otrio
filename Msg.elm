module Msg exposing (..)

import Material
import Model exposing (RackId)


type Msg
    = Mdl (Material.Msg Msg)
    | Select RackId
    | NoOp
