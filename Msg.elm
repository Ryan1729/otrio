module Msg exposing (..)

import Material


type Msg
    = Mdl (Material.Msg Msg)
    | NoOp
