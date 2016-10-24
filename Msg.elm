module Msg exposing (..)

import Material
import Model exposing (RackId, BoardId)


type Msg
    = Mdl (Material.Msg Msg)
    | Select RackId
    | Place BoardId
    | NoOp
