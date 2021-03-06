module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (Model, PieceColour(..), Board, RackId(..), BoardId(..), Size(..), defaultState)
import Material
import Ports
import Extras
import Random.Pcg as Random exposing (Seed)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewGame players ->
            ( { defaultState | players = players, muted = model.muted }, Cmd.none )

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
                                ( cpuMoves { newModel | selected = Nothing }
                                , if model.muted then
                                    Cmd.none
                                  else
                                    Ports.sound "clack"
                                )
            else
                ( model, Cmd.none )

        Mdl msg' ->
            Material.update msg' model

        Select rackId ->
            ( { model | selected = Just rackId }, Cmd.none )

        ToggleMute ->
            ( { model | muted = not model.muted }, Cmd.none )


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
        List.foldl takeTurn model rackList


takeTurn : PieceColour -> Model -> Model
takeTurn pieceColour model =
    let
        maybeWinner =
            Model.getWinner model.board

        maybeMove =
            findMove pieceColour model
    in
        case ( maybeWinner, maybeMove ) of
            ( Nothing, Just ( rackId, boardId ) ) ->
                Model.placeAt pieceColour rackId boardId model
                    |> Maybe.withDefault model

            _ ->
                model


findMove : PieceColour -> Model -> Maybe ( RackId, BoardId )
findMove pieceColour model =
    let
        moves =
            getAvailableMoves pieceColour model
                |> shuffle (Random.initialSeed 42)

        maybeWinningMove =
            Extras.find (winningMove pieceColour model) moves
    in
        case maybeWinningMove of
            Nothing ->
                case
                    let
                        blockingBoardIds =
                            getBlockingBoardIds (otherColours pieceColour) model.board
                    in
                        Extras.find (\( _, boardId ) -> List.member boardId blockingBoardIds) moves
                of
                    Nothing ->
                        Random.step (Random.sample moves) (Random.initialSeed 42)
                            |> fst

                    move ->
                        move

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
            True


getBlockingBoardIds : List PieceColour -> Board -> List BoardId
getBlockingBoardIds pieceColours board =
    List.concatMap (Model.getBlockingBoardIdsForColour board) pieceColours


otherColours : PieceColour -> List PieceColour
otherColours pieceColour =
    List.filter ((/=) pieceColour) Model.pieceColourPossibilities


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
