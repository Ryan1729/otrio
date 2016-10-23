module View exposing (view)

import Model exposing (Model, BoardState(..), Section, Rack)
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Msg exposing (Msg(..))
import Material.Button as Button


red : String
red =
    "#EFABCD"


green : String
green =
    "#CDEFAB"


blue : String
blue =
    "#ABCDEF"


yellow : String
yellow =
    "#EFEFAB"


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            ]
        ]
        [ Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.raised
            , Button.ripple
            , Button.onClick NoOp
            ]
            [ text "New Game" ]
        , model.board
            |> toString
            |> text
        , rackDescription blue model.blueRack
            |> List.map sectionView
            |> div []
        ]


rackDescription : String -> Rack -> List (RingsDescription)
rackDescription colour rack =
    [ rackSectionDescription colour rack.first
    , rackSectionDescription colour rack.second
    , rackSectionDescription colour rack.third
    ]


rackSectionDescription : String -> Section Bool -> RingsDescription
rackSectionDescription colour section =
    RingsDescription (descriptionFromRack colour section.large)
        (descriptionFromRack colour section.medium)
        (descriptionFromRack colour section.small)


boardSectionDescription : Section BoardState -> RingsDescription
boardSectionDescription section =
    RingsDescription (descriptionFromBoardState section.large)
        (descriptionFromBoardState section.medium)
        (descriptionFromBoardState section.small)


sectionView : RingsDescription -> Html Msg
sectionView (RingsDescription largeDescription mediumDescription smallDescription) =
    ( largeDescription, mediumDescription, smallDescription )
        |> toString
        |> text


type RingDescription
    = Colour String
    | Blank


type RingsDescription
    = RingsDescription RingDescription RingDescription RingDescription



-- (,,)


descriptionFromRack : String -> Bool -> RingDescription
descriptionFromRack colour b =
    if b then
        Colour colour
    else
        Blank


descriptionFromBoardState : BoardState -> RingDescription
descriptionFromBoardState boardState =
    case boardState of
        Red ->
            Colour red

        Green ->
            Colour green

        Blue ->
            Colour blue

        Yellow ->
            Colour yellow

        Empty ->
            Blank
