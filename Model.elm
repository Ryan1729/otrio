module Model exposing (..)

import Material


type alias Model =
    { mdl : Material.Model }


defaultState =
    { mdl = Material.model }
