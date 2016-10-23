module View exposing (view)

import Model exposing (Model)
import Html exposing (Html, text)
import Msg exposing (Msg(..))
import Material.Button as Button


view : Model -> Html Msg
view model =
    Html.div []
        [ model
            |> toString
            |> text
        , Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.raised
            , Button.ripple
            , Button.onClick NoOp
            ]
            [ text "test Button" ]
        ]
