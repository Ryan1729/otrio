module Model exposing (..)

import Material


type alias Model =
    { mdl : Material.Model
    , board : Board
    , redRack : Rack
    , greenRack : Rack
    , blueRack : Rack
    , yellowRack : Rack
    }


defaultState =
    { mdl = Material.model
    , board = emptyBoard
    , redRack = fullRack
    , greenRack = fullRack
    , blueRack = fullRack
    , yellowRack = fullRack
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


type alias Section a =
    { large : a
    , medium : a
    , small : a
    }


emptySection : Section BoardState
emptySection =
    Section Empty Empty Empty


type BoardState
    = Red
    | Green
    | Blue
    | Yellow
    | Empty


type alias Rack =
    { first : Section Bool
    , second : Section Bool
    , third : Section Bool
    }


emptyRack =
    Rack emptyRackSection emptyRackSection emptyRackSection


emptyRackSection =
    Section False False False


fullRack =
    Rack fullRackSection fullRackSection fullRackSection


fullRackSection =
    Section True True True
