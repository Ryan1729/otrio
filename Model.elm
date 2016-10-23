module Model exposing (..)

import Material


type alias Model =
    { mdl : Material.Model
    , board : Board
    }


defaultState =
    { mdl = Material.model
    , board = emptyBoard
    }


emptyBoard : Board
emptyBoard =
    Board emptyBoardSection emptyBoardSection emptyBoardSection


type alias Board =
    { large : BoardSection
    , medium : BoardSection
    , small : BoardSection
    }


emptyBoardSection : BoardSection
emptyBoardSection =
    BoardSection Empty
        Empty
        Empty
        Empty
        Empty
        Empty
        Empty
        Empty
        Empty


type alias BoardSection =
    { topLeft : SectionState
    , topMiddle : SectionState
    , topRight : SectionState
    , leftMiddle : SectionState
    , middle : SectionState
    , rightMiddle : SectionState
    , bottomLeft : SectionState
    , bottomMiddle : SectionState
    , bottomRight : SectionState
    }


type SectionState
    = Red
    | Blue
    | Green
    | Yellow
    | Empty
