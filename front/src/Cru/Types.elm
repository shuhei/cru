module Cru.Types exposing (..)

import Json.Decode as Json exposing (Decoder)


type Nickname
    = Nickname String


type Hostname
    = Hostname String


type Username
    = Username String


type Target
    = NickTarget Nickname
    | ServerTarget Hostname


type Reply
    = WelcomeReply
    | OtherReply


type Address
    = IPv4 String
    | IPv6 String


type Host
    = HostByName Hostname
    | HostByAddr Address
    | HostCloak String


type Prefix
    = PrefixServer Hostname
    | PrefixNick Nickname (Maybe Username) (Maybe Host)


type Message
    = OtherMessage


type alias SpecificReply =
    { hostname : Hostname
    , target : Target
    , reply : Reply
    }


type alias SpecificMessage =
    { prefix : Maybe Prefix
    , message : Message
    }



-- TODO: data MessageOrReply = SpecificMessage | SpecificReply ?


type Either a b
    = Left a
    | Right b



-- DECODERS


withType : String -> Decoder a -> Decoder a
withType t d =
    let
        checkType x =
            if t == x then
                d
            else
                Json.fail "type does not match"
    in
        Json.field "type" Json.string
            |> Json.andThen checkType


decodeNickName : Decoder Nickname
decodeNickName =
    Json.map Nickname Json.string


decodeHostname : Decoder Hostname
decodeHostname =
    Json.map Hostname Json.string


decodeUsername : Decoder Username
decodeUsername =
    Json.map Username Json.string


decodeTarget : Decoder Target
decodeTarget =
    Json.oneOf
        [ withType "Nick" <|
            Json.map NickTarget (Json.field "value" decodeNickName)
        , withType "Server" <|
            Json.map ServerTarget (Json.field "value" decodeHostname)
        ]


decodeReply : Decoder Reply
decodeReply =
    Json.oneOf
        [ withType "Welcome" <| Json.succeed WelcomeReply
        , withType "Other" <| Json.succeed OtherReply
        ]


decodeAddress : Decoder Address
decodeAddress =
    Json.oneOf
        [ withType "v4" <|
            Json.map IPv4 (Json.field "ip" Json.string)
        , withType "v6" <|
            Json.map IPv6 (Json.field "ip" Json.string)
        ]


decodeHost : Decoder Host
decodeHost =
    Json.oneOf
        [ withType "ByName" <|
            Json.map HostByName (Json.field "hostname" decodeHostname)
        , withType "ByAddr" <|
            Json.map HostByAddr (Json.field "address" decodeAddress)
        , withType "Cloak" <|
            Json.map HostCloak (Json.field "cloak" Json.string)
        ]


decodePrefix : Decoder Prefix
decodePrefix =
    Json.oneOf
        [ withType "Server" <|
            Json.map PrefixServer
                (Json.field "hostname" decodeHostname)
        , withType "Nick" <|
            Json.map3 PrefixNick
                (Json.field "nickname" decodeNickName)
                (Json.maybe <| Json.field "username" decodeUsername)
                (Json.maybe <| Json.field "host" decodeHost)
        ]


decodeMessage : Decoder Message
decodeMessage =
    Json.oneOf
        [ withType "Other" <| Json.succeed OtherMessage
        ]


decodeSpecificMessage : Decoder SpecificMessage
decodeSpecificMessage =
    Json.map2 SpecificMessage
        (Json.maybe <| Json.field "prefix" decodePrefix)
        (Json.field "message" decodeMessage)


decodeSpecificReply : Decoder SpecificReply
decodeSpecificReply =
    Json.map3 SpecificReply
        (Json.field "hostname" decodeHostname)
        (Json.field "target" decodeTarget)
        (Json.field "reply" decodeReply)


decodeMessageOrReply : Decoder (Either SpecificReply SpecificMessage)
decodeMessageOrReply =
    Json.oneOf
        [ Json.map Left <| Json.field "Left" decodeSpecificReply
        , Json.map Right <| Json.field "Right" decodeSpecificMessage
        ]
