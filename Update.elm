module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (Model, PieceColour(..), Board, RackId(..), BoardId(..), Size(..))
import Material
import Ports
import Extras
import Random.Pcg as Random exposing (Seed)


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
                                ( cpuMoves { newModel | selected = Nothing }, Ports.sound "clack" )
            else
                ( model, Cmd.none )

        Mdl msg' ->
            Material.update msg' model

        Select rackId ->
            ( { model | selected = Just rackId }, Cmd.none )


cpuMoves : Model -> Model
cpuMoves model =
    let
        rackList =
            case model.players of
                2 ->
                    [ Red ]

                3 ->
                    [ Green, Red ]

                _ ->
                    [ Green, Red, Yellow ]
    in
        List.foldl takeMove model rackList


takeMove : PieceColour -> Model -> Model
takeMove pieceColour model =
    let
        maybeMove =
            findMove pieceColour model
    in
        case maybeMove of
            Just ( rackId, boardId ) ->
                Model.placeAt pieceColour rackId boardId model
                    |> Maybe.withDefault model

            Nothing ->
                model


findMove : PieceColour -> Model -> Maybe ( RackId, BoardId )
findMove pieceColour model =
    let
        moves =
            getAvailableMoves pieceColour model
                |> shuffle (Random.initialSeed 42)

        maybeWinningMove =
            moves
                |> Extras.find (winningMove pieceColour model)
    in
        case maybeWinningMove of
            Nothing ->
                Random.step (Random.sample moves) (Random.initialSeed 42)
                    |> fst

            move ->
                move


winningMove : PieceColour -> Model -> ( RackId, BoardId ) -> Bool
winningMove pieceColour model ( rackId, boardId ) =
    case
        Model.placeAt pieceColour rackId boardId model
            `Maybe.andThen` (Model.getWinner << .board)
    of
        Nothing ->
            False

        Just a ->
            let
                _ =
                    Debug.log "a" a
            in
                True
                    |> Debug.log ""


shuffle : Seed -> List a -> List a
shuffle seed list =
    let
        length =
            List.length list

        randomTags =
            Random.step (Random.list length (Random.int 0 length)) (Random.initialSeed 42)
                |> fst
    in
        List.map2 (,) randomTags list |> List.sortBy fst |> List.unzip |> snd


getAvailableMoves : PieceColour -> Model -> List ( RackId, BoardId )
getAvailableMoves pieceColour model =
    getAvailableRackIds pieceColour model
        |> atMostOneOfEachSize
        |> List.concatMap (getAvailableMovesForRackId model.board)


getAvailableRackIds pieceColour model =
    let
        rack =
            Model.getRackByPieceColour pieceColour model
    in
        List.filterMap
            (\rackId ->
                if Model.getRackSectionValue pieceColour rackId model then
                    Just rackId
                else
                    Nothing
            )
            Model.rackIdPossibilities


atMostOneOfEachSize : List RackId -> List RackId
atMostOneOfEachSize rackIds =
    [ Extras.find isLarge rackIds
    , Extras.find isMedium rackIds
    , Extras.find isSmall rackIds
    ]
        |> List.filterMap identity


isLarge : RackId -> Bool
isLarge (RackId _ size) =
    size == Large


isMedium : RackId -> Bool
isMedium (RackId _ size) =
    size == Medium


isSmall : RackId -> Bool
isSmall (RackId _ size) =
    size == Small


getAvailableMovesForRackId : Board -> RackId -> List ( RackId, BoardId )
getAvailableMovesForRackId board rackId =
    let
        (RackId _ size) =
            rackId
    in
        Model.getAllEmptySpacesOfSize board size
            |> List.map ((,) rackId)
