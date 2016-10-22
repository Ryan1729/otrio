module Otrio exposing (..)

import Html.App exposing (program)
import Model exposing (defaultState)
import View exposing (view)
import Model exposing (Model)
import Msg exposing (Msg)
import Update exposing (update)


init : ( Model, Cmd Msg )
init =
    defaultState ! []


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
