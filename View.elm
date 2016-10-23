module View exposing (view)

import Model exposing (Model, BoardState(..), Section, Rack)
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Msg exposing (Msg(..))
import Material.Button as Button
import Svg exposing (Svg, svg, circle, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


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


gameWidth : Int
gameWidth =
    600


gameHeight : Int
gameHeight =
    600


widthString =
    toString gameWidth


heightString =
    toString gameHeight


sectionWidth : Int
sectionWidth =
    gameWidth // 5


sectionHeight : Int
sectionHeight =
    gameHeight // 5


sectionWidthString : String
sectionWidthString =
    toString sectionWidth


sectionHeightString : String
sectionHeightString =
    toString sectionHeight


halfSectionWidth : Int
halfSectionWidth =
    sectionWidth // 2


halfSectionHeight : Int
halfSectionHeight =
    sectionHeight // 2


halfSectionWidthString : String
halfSectionWidthString =
    toString halfSectionWidth


halfSectionHeightString : String
halfSectionHeightString =
    toString halfSectionHeight


viewBoxString =
    "-"
        ++ halfSectionWidthString
        ++ " -"
        ++ halfSectionHeightString
        ++ " "
        ++ sectionWidthString
        ++ " "
        ++ sectionHeightString


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style
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
            |> List.map (sectionView model)
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


sectionView : Model -> RingsDescription -> Html Msg
sectionView model (RingsDescription largeDescription mediumDescription smallDescription) =
    div []
        [ svg [ width sectionWidthString, height sectionHeightString, viewBox viewBoxString ] [ circle [ r "30", fill "#7F9Fff" ] [] ]
        , ( mediumDescription, smallDescription )
            |> toString
            |> text
        ]


type RingDescription
    = Colour String
    | Blank


type RingsDescription
    = RingsDescription RingDescription RingDescription RingDescription


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
