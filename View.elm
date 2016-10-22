module View exposing (view)

import Model exposing (Model)
import Html exposing (Html)
import Msg exposing (Msg)


view : Model -> Html Msg
view model =
    model
        |> toString
        |> Html.text
