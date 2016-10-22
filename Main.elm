module Main exposing (..)

import Html.App exposing (program)
import Html exposing (Html)


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    {} ! []


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    model
        |> toString
        |> Html.text


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
