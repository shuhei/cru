module Main exposing (..)

import Html exposing (Html, text, div, input, p, button)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onClick)
import WebSocket as WS


---- MODEL ----


type alias Model =
    { connected : Bool
    , loggedIn : Bool
    , nickname : String
    , channel : String
    , lines : List String
    }


initialModel : Model
initialModel =
    { connected = False
    , loggedIn = False
    , nickname = "test"
    , channel = "#tutbot-testing"
    , lines = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | IncomingMessage String
    | ChangeNickname String
    | ChangeChannel String
    | LogIn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        IncomingMessage message ->
            handleMessage message model

        ChangeNickname nickname ->
            ( { model | nickname = nickname }, Cmd.none )

        ChangeChannel channel ->
            ( { model | channel = channel }, Cmd.none )

        LogIn ->
            ( model, sendMessage "login" )


handleMessage : String -> Model -> ( Model, Cmd Msg )
handleMessage message model =
    case message of
        "connected" -> ( { model | connected = True }, Cmd.none )
        _ -> ( model, Cmd.none )


sendMessage : String -> Cmd msg
sendMessage =
    WS.send wsEndpoint


---- VIEW ----


view : Model -> Html Msg
view model =
    if not model.connected then
        p [] [ text "Connecting..." ]
    else if not model.loggedIn then
        loginForm model
    else
        p [] [ text "Logged in!" ]


loginForm : Model -> Html Msg
loginForm model =
    div []
        [ input
            [ value <| model.nickname, onInput ChangeNickname ]
            []
        , input
            [ value <| model.channel, onInput ChangeChannel ]
            []
        , button
            [ onClick LogIn ]
            [ text "Log in" ]
        ]


---- SUBSCRIPTION ----


wsEndpoint : String
wsEndpoint = "ws://localhost:8080"


subscriptions : Model -> Sub Msg
subscriptions _ =
    WS.listen wsEndpoint IncomingMessage


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
