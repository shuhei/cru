module Main exposing (..)

import Html exposing (Html, Attribute, text, div, span, input, p, button, i, label)
import Html.Attributes exposing (value, class, type_, placeholder)
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
        "connected" ->
            ( { model | connected = True }, Cmd.none )

        "loggedin" ->
            ( { model | loggedIn = True }, Cmd.none )

        _ ->
            ( { model | lines = message :: model.lines }, Cmd.none )


sendMessage : String -> Cmd msg
sendMessage =
    WS.send wsEndpoint



---- VIEW ----


view : Model -> Html Msg
view model =
    if not model.connected then
        p [] [ text "Connecting..." ]
    else if not model.loggedIn then
        viewLoginForm model
    else
        viewLines model.lines


viewLines : List String -> Html Msg
viewLines lines =
    let
        viewLine line =
            p [] [ text line ]
    in
        div [] <| List.map viewLine (List.reverse lines)


inputField : String -> List (Attribute msg) -> Html msg
inputField name attrs =
    div [ class "field" ]
        [ label [ class "label" ] [ text name ]
        , div
            [ class "control" ]
            [ input ([ class "input", type_ "text" ] ++ attrs) []
            ]
        ]


viewLoginForm : Model -> Html Msg
viewLoginForm model =
    div [ class "box login-form" ]
        [ inputField "Nickname" [ value <| model.nickname, onInput ChangeNickname ]
        , inputField "Channel"[ value <| model.channel, onInput ChangeChannel ]
        , div
            [ class "control" ]
            [ button [ class "button is-primary", onClick LogIn ]
                [ text "Log in" ]
            ]
        ]



---- SUBSCRIPTION ----


wsEndpoint : String
wsEndpoint =
    "ws://localhost:8080"


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
