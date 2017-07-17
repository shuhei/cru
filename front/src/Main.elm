module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src)
import WebSocket as WS


---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | SocketMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        SocketMessage message ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Your Elm App is working!" ]
        ]


---- SUBSCRIPTION ----


wsEndpoint : String
wsEndpoint = "ws://localhost:8080"


subscriptions : Model -> Sub Msg
subscriptions _ =
    WS.listen wsEndpoint SocketMessage


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
