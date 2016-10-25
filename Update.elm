module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (Model, PieceColour(..))
import Material
import Ports


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Place boardId ->
            if Model.isFree model.board boardId then
                case model.selected of
                    Nothing ->
                        ( model, Cmd.none )

                    Just rackId ->
                        case Model.placeAt Blue rackId boardId model of
                            Nothing ->
                                ( model, Cmd.none )

                            Just newModel ->
                                ( { newModel | selected = Nothing }, Ports.sound "clack" )
            else
                ( model, Cmd.none )

        Mdl msg' ->
            Material.update msg' model

        Select rackId ->
            ( { model | selected = Just rackId }, Cmd.none )
