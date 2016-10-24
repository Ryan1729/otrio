module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (Model)
import Material
import Ports


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Place ->
            ( model, Ports.sound "clack" )

        Mdl msg' ->
            Material.update msg' model

        Select rackId ->
            ( { model | selected = Just rackId }, Cmd.none )
