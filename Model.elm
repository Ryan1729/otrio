module Model exposing (..)

import Material
import Extras


type alias Model =
    { mdl : Material.Model
    , board : Board
    , redRack : Rack
    , greenRack : Rack
    , blueRack : Rack
    , yellowRack : Rack
    , selected : Maybe RackId
    , players : Int
    }


defaultState =
    { mdl = Material.model
    , board = emptyBoard
    , redRack = fullRack
    , greenRack = fullRack
    , blueRack = fullRack
    , yellowRack = fullRack
    , selected = Nothing
    , players = 3
    }


emptyBoard : Board
emptyBoard =
    Board emptySection
        emptySection
        emptySection
        emptySection
        emptySection
        emptySection
        emptySection
        emptySection
        emptySection


type alias Board =
    { topLeft : Section BoardState
    , topMiddle : Section BoardState
    , topRight : Section BoardState
    , leftMiddle : Section BoardState
    , middle : Section BoardState
    , rightMiddle : Section BoardState
    , bottomLeft : Section BoardState
    , bottomMiddle : Section BoardState
    , bottomRight : Section BoardState
    }


type BoardId
    = BoardId BoardLocation Size


type BoardLocation
    = TopLeft
    | TopMiddle
    | TopRight
    | LeftMiddle
    | Middle
    | RightMiddle
    | BottomLeft
    | BottomMiddle
    | BottomRight


boardLocationPossibilities : List BoardLocation
boardLocationPossibilities =
    [ TopLeft
    , TopMiddle
    , TopRight
    , LeftMiddle
    , Middle
    , RightMiddle
    , BottomLeft
    , BottomMiddle
    , BottomRight
    ]


boardIdPossibilities =
    List.concatMap (\location -> List.map (BoardId location) sizePossibilities) boardLocationPossibilities


lineLocations : List (List BoardLocation)
lineLocations =
    [ --rows
      [ TopLeft
      , TopMiddle
      , TopRight
      ]
    , [ LeftMiddle
      , Middle
      , RightMiddle
      ]
    , [ BottomLeft
      , BottomMiddle
      , BottomRight
      ]
      --columns
    , [ TopLeft
      , LeftMiddle
      , BottomLeft
      ]
    , [ TopMiddle
      , Middle
      , BottomMiddle
      ]
    , [ TopRight
      , RightMiddle
      , BottomRight
      ]
      --diagonals
    , [ TopLeft
      , Middle
      , BottomRight
      ]
    , [ TopRight
      , Middle
      , BottomLeft
      ]
    ]


getBoardSection : Board -> BoardLocation -> Section BoardState
getBoardSection board boardLocation =
    case boardLocation of
        TopLeft ->
            board.topLeft

        TopMiddle ->
            board.topMiddle

        TopRight ->
            board.topRight

        LeftMiddle ->
            board.leftMiddle

        Middle ->
            board.middle

        RightMiddle ->
            board.rightMiddle

        BottomLeft ->
            board.bottomLeft

        BottomMiddle ->
            board.bottomMiddle

        BottomRight ->
            board.bottomRight


type alias Section a =
    { large : a
    , medium : a
    , small : a
    }


getSectionValue : Size -> Section a -> a
getSectionValue size section =
    case size of
        Large ->
            section.large

        Medium ->
            section.medium

        Small ->
            section.small


setSectionValue : Size -> a -> Section a -> Section a
setSectionValue size thing section =
    case size of
        Large ->
            { section | large = thing }

        Medium ->
            { section | medium = thing }

        Small ->
            { section | small = thing }


emptySection : Section BoardState
emptySection =
    Section Empty Empty Empty


type BoardState
    = PieceType PieceColour
    | Empty


type PieceColour
    = Red
    | Green
    | Blue
    | Yellow


pieceColourPossibilities =
    [ Red
    , Green
    , Blue
    , Yellow
    ]


type alias Rack =
    { first : Section Bool
    , second : Section Bool
    , third : Section Bool
    }


getRackSection : Int -> Rack -> Section Bool
getRackSection index rack =
    case index of
        0 ->
            rack.first

        1 ->
            rack.second

        _ ->
            rack.third


setRackSection (RackId index size) newValue rack =
    case index of
        0 ->
            { rack | first = setSectionValue size newValue rack.first }

        1 ->
            { rack | second = setSectionValue size newValue rack.second }

        _ ->
            { rack | third = setSectionValue size newValue rack.third }


emptyRack =
    Rack emptyRackSection emptyRackSection emptyRackSection


emptyRackSection =
    Section False False False


fullRack =
    Rack fullRackSection fullRackSection fullRackSection


fullRackSection =
    Section True True True


type RackId
    = RackId Int Size


rackIndexPossibilities =
    [0..2]


rackIdPossibilities =
    rackIndexPossibilities
        |> List.concatMap
            (\index ->
                List.map (RackId index) sizePossibilities
            )


type Size
    = Large
    | Medium
    | Small


sizePossibilities =
    [ Large, Medium, Small ]


placeAt : PieceColour -> RackId -> BoardId -> Model -> Maybe Model
placeAt pieceColour rackId boardId model =
    if sizesMatch rackId boardId && getRackSectionValue pieceColour rackId model then
        let
            postRackSetModel =
                setRackSectionValue pieceColour rackId False model
        in
            Just { postRackSetModel | board = setBoardSectionValue pieceColour boardId postRackSetModel.board }
    else
        Nothing


sizesMatch : RackId -> BoardId -> Bool
sizesMatch (RackId _ rackSize) (BoardId _ boardSize) =
    rackSize == boardSize


setBoardSectionValue : PieceColour -> BoardId -> Board -> Board
setBoardSectionValue pieceColour (BoardId location size) board =
    case location of
        TopLeft ->
            { board | topLeft = setSectionValue size (PieceType pieceColour) board.topLeft }

        TopMiddle ->
            { board | topMiddle = setSectionValue size (PieceType pieceColour) board.topMiddle }

        TopRight ->
            { board | topRight = setSectionValue size (PieceType pieceColour) board.topRight }

        LeftMiddle ->
            { board | leftMiddle = setSectionValue size (PieceType pieceColour) board.leftMiddle }

        Middle ->
            { board | middle = setSectionValue size (PieceType pieceColour) board.middle }

        RightMiddle ->
            { board | rightMiddle = setSectionValue size (PieceType pieceColour) board.rightMiddle }

        BottomLeft ->
            { board | bottomLeft = setSectionValue size (PieceType pieceColour) board.bottomLeft }

        BottomMiddle ->
            { board | bottomMiddle = setSectionValue size (PieceType pieceColour) board.bottomMiddle }

        BottomRight ->
            { board | bottomRight = setSectionValue size (PieceType pieceColour) board.bottomRight }


getRackSectionValue : PieceColour -> RackId -> Model -> Bool
getRackSectionValue pieceColour (RackId index size) model =
    model
        |> getRackByPieceColour pieceColour
        |> getRackSection index
        |> getSectionValue size


setRackSectionValue : PieceColour -> RackId -> Bool -> Model -> Model
setRackSectionValue pieceColour rackId newValue model =
    case pieceColour of
        Red ->
            { model | redRack = setRackSection rackId newValue model.redRack }

        Green ->
            { model | greenRack = setRackSection rackId newValue model.greenRack }

        Blue ->
            { model | blueRack = setRackSection rackId newValue model.blueRack }

        Yellow ->
            { model | yellowRack = setRackSection rackId newValue model.yellowRack }


getRackByPieceColour pieceColour model =
    case pieceColour of
        Red ->
            model.redRack

        Green ->
            model.greenRack

        Blue ->
            model.blueRack

        Yellow ->
            model.yellowRack


isFree : Board -> BoardId -> Bool
isFree board (BoardId location size) =
    (location
        |> getBoardSection board
        |> getSectionValue size
    )
        == Empty


getWinner : Board -> Maybe PieceColour
getWinner board =
    Extras.find (hasMatchOfGivenColour board) pieceColourPossibilities


hasMatchOfGivenColour : Board -> PieceColour -> Bool
hasMatchOfGivenColour board pieceColour =
    let
        boardState =
            PieceType pieceColour
    in
        List.any (completeSection boardState) (getBoardSections board)
            || let
                lines =
                    getLines board
               in
                List.any (matchingLine boardState) lines
                    || List.any (oneOfEachLine boardState) lines


getBoardSections : Board -> List (Section BoardState)
getBoardSections board =
    List.map (getBoardSection board) boardLocationPossibilities


getLines : Board -> List (List (Section BoardState))
getLines board =
    List.map (List.map (getBoardSection board)) lineLocations


matchingLine : BoardState -> List (Section BoardState) -> Bool
matchingLine boardState line =
    List.all (.large >> (==) boardState) line
        || List.all (.medium >> (==) boardState) line
        || List.all (.small >> (==) boardState) line


oneOfEachLine : BoardState -> List (Section BoardState) -> Bool
oneOfEachLine boardState line =
    case line of
        first :: second :: third :: _ ->
            --TODO remove duplicate record acceses
            (first.small == boardState && second.medium == boardState && third.large == boardState)
                || (first.small == boardState && second.large == boardState && third.medium == boardState)
                || (first.medium == boardState && second.small == boardState && third.large == boardState)
                || (first.medium == boardState && second.large == boardState && third.small == boardState)
                || (first.large == boardState && second.small == boardState && third.medium == boardState)
                || (first.large == boardState && second.medium == boardState && third.small == boardState)

        _ ->
            False


completeSection : BoardState -> Section BoardState -> Bool
completeSection boardState section =
    (section.large == boardState)
        && (section.medium == boardState)
        && (section.small == boardState)


getAllEmptySpacesOfSize : Board -> Size -> List (BoardId)
getAllEmptySpacesOfSize board size =
    List.filterMap
        (\location ->
            let
                boardId =
                    BoardId location size
            in
                case isFree board boardId of
                    True ->
                        Just boardId

                    False ->
                        Nothing
        )
        boardLocationPossibilities


getBlockingBoardIdsForColour : Board -> PieceColour -> List BoardId
getBlockingBoardIdsForColour board pieceColour =
    List.filter
        (\boardId ->
            case
                setBoardSectionValue pieceColour boardId board
                    |> getWinner
            of
                Just _ ->
                    True

                Nothing ->
                    False
        )
        boardIdPossibilities
