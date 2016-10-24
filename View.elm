module View exposing (view)

import Model exposing (Model, Board, BoardState(..), Section, Rack, RackId(..), Size(..))
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Msg exposing (Msg(..))
import Material.Options as Options exposing (css)
import Material.Button as Button
import Svg exposing (Svg, svg, circle, polygon, Attribute)
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


gameWidth : Float
gameWidth =
    600


gameHeight : Float
gameHeight =
    600


widthString =
    toString gameWidth


heightString =
    toString gameHeight


sectionWidth : Float
sectionWidth =
    gameWidth / 5


sectionHeight : Float
sectionHeight =
    gameHeight / 5


sectionWidthString : String
sectionWidthString =
    toString sectionWidth


sectionHeightString : String
sectionHeightString =
    toString sectionHeight


halfSectionWidth : Float
halfSectionWidth =
    sectionWidth / 2


halfSectionHeight : Float
halfSectionHeight =
    sectionHeight / 2


halfSectionWidthString : String
halfSectionWidthString =
    toString halfSectionWidth


halfSectionHeightString : String
halfSectionHeightString =
    toString halfSectionHeight


viewBoxString =
    "0 0 "
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
        [ div
            [ Html.Attributes.style
                [ ( "display", "flex" )
                , ( "flex-direction", "row" )
                ]
            ]
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.raised
                , Button.ripple
                , Button.onClick NoOp
                , css "width" (sectionWidthString ++ "px")
                ]
                [ text "New Game" ]
            , rackDescription red model.redRack
                |> List.map sectionView
                |> div
                    [ Html.Attributes.style
                        [ ( "display", "flex" )
                        , ( "flex-direction", "row" )
                        ]
                    ]
            , Button.render Mdl
                [ 1 ]
                model.mdl
                [ Button.raised
                , Button.ripple
                , Button.onClick NoOp
                , css "width" (sectionWidthString ++ "px")
                ]
                [ text "Mute" ]
            ]
        , div
            [ Html.Attributes.style
                [ ( "display", "flex" )
                , ( "flex-direction", "row" )
                ]
            ]
            [ rackDescription green model.greenRack
                |> List.map sectionView
                |> div
                    [ Html.Attributes.style
                        [ ( "display", "flex" )
                        , ( "flex-direction", "column" )
                        ]
                    ]
            , boardView model.board
            , rackDescription yellow model.yellowRack
                |> List.map sectionView
                |> div
                    [ Html.Attributes.style
                        [ ( "display", "flex" )
                        , ( "flex-direction", "column" )
                        ]
                    ]
            ]
        , rackDescription blue model.blueRack
            |> List.indexedMap (playerRackSectionView model.selected)
            |> div
                [ Html.Attributes.style
                    [ ( "display", "flex" )
                    , ( "flex-direction", "row" )
                    ]
                ]
        ]


boardView : Board -> Html Msg
boardView board =
    div
        [ Html.Attributes.style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            ]
        ]
        [ boardRowView board.topLeft board.topMiddle board.topRight
        , boardRowView board.leftMiddle board.middle board.rightMiddle
        , boardRowView board.bottomLeft board.bottomMiddle board.bottomRight
        ]


boardRowView : Section BoardState -> Section BoardState -> Section BoardState -> Html Msg
boardRowView left middle right =
    div
        [ Html.Attributes.style
            [ ( "display", "flex" )
            , ( "flex-direction", "row" )
            ]
        ]
        [ left
            |> boardSectionDescription
            |> sectionView
        , middle
            |> boardSectionDescription
            |> sectionView
        , right
            |> boardSectionDescription
            |> sectionView
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
    svg [ width sectionWidthString, height sectionHeightString, viewBox viewBoxString ]
        [ case largeDescription of
            Colour colour ->
                piecePath [ stroke "grey", fill colour, d largeRing ]

            Blank ->
                piecePath [ onClick Place, fillOpacity "0.1", d largeRing, stroke "grey" ]
        , case mediumDescription of
            Colour colour ->
                piecePath [ stroke "grey", fill colour, d mediumRing ]

            Blank ->
                piecePath [ onClick Place, fillOpacity "0.1", d mediumRing, stroke "grey" ]
        , case smallDescription of
            Colour colour ->
                circle
                    [ cx halfSectionWidthString
                    , cy halfSectionWidthString
                    , r <| toString (halfSectionWidth * 1 / 9)
                    , stroke "grey"
                    , fill colour
                    ]
                    []

            Blank ->
                circle
                    [ cx halfSectionWidthString
                    , cy halfSectionWidthString
                    , r <| toString (halfSectionWidth * 1 / 9)
                    , stroke "grey"
                    , onClick Place
                    , fillOpacity "0.1"
                    ]
                    []
        ]


playerRackSectionView : Maybe RackId -> Int -> RingsDescription -> Html Msg
playerRackSectionView maybeRackId index (RingsDescription largeDescription mediumDescription smallDescription) =
    let
        curriedHelper =
            playerRackSectionViewHelper index largeDescription mediumDescription smallDescription
    in
        case maybeRackId of
            Nothing ->
                curriedHelper noHighlight noHighlight noHighlight

            Just (RackId rackIndex size) ->
                if index == rackIndex then
                    case size of
                        Large ->
                            curriedHelper highlight noHighlight noHighlight

                        Medium ->
                            curriedHelper noHighlight highlight noHighlight

                        Small ->
                            curriedHelper noHighlight noHighlight highlight
                else
                    curriedHelper noHighlight noHighlight noHighlight


noHighlight =
    [ stroke "grey" ]


highlight =
    [ stroke "white" ]


playerRackSectionViewHelper index largeDescription mediumDescription smallDescription largeAttributes mediumAttributes smallAttributes =
    svg [ width sectionWidthString, height sectionHeightString, viewBox viewBoxString ]
        [ case largeDescription of
            Colour colour ->
                [ RackId index Large |> Select |> onClick, fill colour, d largeRing ]
                    ++ largeAttributes
                    |> piecePath

            Blank ->
                nullSvg
        , case mediumDescription of
            Colour colour ->
                [ RackId index Medium |> Select |> onClick, fill colour, d mediumRing ]
                    ++ mediumAttributes
                    |> piecePath

            Blank ->
                nullSvg
        , case smallDescription of
            Colour colour ->
                [ RackId index Small |> Select |> onClick, cx halfSectionWidthString, cy halfSectionWidthString, r <| toString (halfSectionWidth * 1 / 9), stroke "grey", fill colour ]
                    ++ smallAttributes
                    |> pieceCircle

            Blank ->
                nullSvg
        ]


piecePath : List (Svg.Attribute Msg) -> Svg Msg
piecePath attributes =
    Svg.path attributes []


pieceCircle : List (Svg.Attribute Msg) -> Svg Msg
pieceCircle attributes =
    circle attributes []


nullSvg =
    piecePath []


largeRing : String
largeRing =
    donut (halfSectionWidth + 0.5) (halfSectionHeight + 0.5) (halfSectionWidth - 1.5) (halfSectionWidth * 7 / 9)


mediumRing : String
mediumRing =
    donut (halfSectionWidth + 0.5) (halfSectionHeight + 0.5) ((halfSectionWidth * 5 / 9) - 1.5) (halfSectionWidth * 3 / 9)


donut : Float -> Float -> Float -> Float -> String
donut x y outerRadius innerRadius =
    -- http://stackoverflow.com/a/37883328/4496839
    let
        outerRadiusString =
            toString outerRadius

        innerRadiusString =
            toString innerRadius
    in
        -- Move to center of ring
        ("M " ++ toString x ++ " " ++ toString y)
            -- Move to top of ring
            ++
                (" m 0 " ++ toString -outerRadius)
            -- Draw outer arc, but don't close it
            ++
                (" a " ++ outerRadiusString ++ " " ++ outerRadiusString ++ " 0 1 0 1 0")
            -- default fill-rule:even-odd will help create the empty innards
            ++
                " Z"
            -- Move to top point of inner radius
            ++
                (" m 0 " ++ toString (outerRadius - innerRadius))
            -- Draw inner arc, but don't close it
            ++
                (" a " ++ innerRadiusString ++ " " ++ innerRadiusString ++ " 0 1 1 -1 0")
            --Close the inner ring. Actually will still work without, but inner ring will have one unit missing in stroke
            ++
                " Z"


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
