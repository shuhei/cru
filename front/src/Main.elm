module Main exposing (..)

import Dom.Scroll as Scroll
import Json.Decode as Json
import Html exposing (Html, Attribute, text, div, span, input, p, button, i, label)
import Html.Attributes exposing (value, class, id, type_, placeholder)
import Html.Events as E
import String
import Task
import WebSocket as WS


---- MODEL ----


type alias Model =
    { wsConnected : Bool
    , loggedIn : Bool
    , nickname : String
    , channel : String
    , lines : List String
    , chatMessage : String
    }


initialModel : Model
initialModel =
    { wsConnected = False
    , loggedIn = False
    , nickname = "test"
    , channel = "#tutbot-testing"
    , lines = []
    , chatMessage = ""
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
    | ChangeChatMessage String
    | SendChatMessage
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

        ChangeChatMessage message ->
            ( { model | chatMessage = message }, Cmd.none )

        SendChatMessage ->
            if String.isEmpty model.chatMessage then
                ( model, Cmd.none )
            else
                ( { model | chatMessage = "" }, sendMessage model.chatMessage )

        LogIn ->
            let
                message =
                    "login " ++ model.nickname ++ " " ++ model.channel
            in
                ( model, sendMessage message )


handleMessage : String -> Model -> ( Model, Cmd Msg )
handleMessage message model =
    case message of
        "connected" ->
            ( { model | wsConnected = True }, Cmd.none )

        "loggedin" ->
            ( { model | loggedIn = True }, Cmd.none )

        _ ->
            ( { model | lines = message :: model.lines }, scrollToBottom )


sendMessage : String -> Cmd msg
sendMessage =
    WS.send wsEndpoint


scrollToBottom : Cmd Msg
scrollToBottom =
    Task.attempt (always NoOp) <| Scroll.toBottom linesContainerId



---- VIEW ----


view : Model -> Html Msg
view model =
    if not model.wsConnected then
        p [] [ text "Connecting..." ]
    else if not model.loggedIn then
        viewLoginForm model
    else
        div [ class "chat-container" ]
            [ chatHeader model.nickname model.channel
            , viewLines model.lines
            , chatBox model.chatMessage
            ]


chatHeader : String -> String -> Html msg
chatHeader nickname channel =
    div [ class "chat-header" ]
        [ text <| nickname ++ " " ++ channel ]


chatBox : String -> Html Msg
chatBox message =
    div [ class "chat-box" ]
        [ div
            [ class "field" ]
            [ div
                [ class "control" ]
                [ input
                    [ class "input"
                    , type_ "text"
                    , value message
                    , E.onInput ChangeChatMessage
                    , onEnter SendChatMessage
                    ]
                    []
                ]
            ]
        ]


linesContainerId : String
linesContainerId =
    "chat-lines"


viewLines : List String -> Html Msg
viewLines lines =
    let
        viewLine line =
            p [] [ text line ]
    in
        div [ class "chat-lines", id linesContainerId ] <|
            List.map viewLine (List.reverse lines)


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
        [ inputField "Nickname" [ value <| model.nickname, E.onInput ChangeNickname ]
        , inputField "Channel" [ value <| model.channel, E.onInput ChangeChannel ]
        , div
            [ class "control" ]
            [ button [ class "button is-primary", E.onClick LogIn ]
                [ text "Log in" ]
            ]
        ]


onEnter : msg -> Attribute msg
onEnter tagger =
    let
        isEnter keyCode =
            if keyCode == 13 then
                Json.succeed tagger
            else
                Json.fail <| toString keyCode
    in
        E.on "keyup" <| Json.andThen isEnter E.keyCode



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
