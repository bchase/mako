module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http



--- init / subs ---


type Lang
    = JP
    | EN


type alias Model =
    { view : View
    , lang : Lang
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    cmds [] <| initModel flags


initModel : Flags -> Model
initModel flags =
    { view = Disclaimer
    , lang = JP
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



--- update ---


type Msg
    = NoOp
    | SetLang Lang
    | SetView View
    | Post Msg


type View
    = Disclaimer
    | BF
    | IT


type DisclaimerResp
    = DisclaimerYes
    | DisclaimerNo


type BFResp
    = BFYes
    | BFNo


type ITResp
    = ITYes
    | ITNo


type Resp
    = DisclaimerResp_ DisclaimerResp
    | BFResp_ BFResp
    | ITResp_ ITResp


redirect : Resp -> View
redirect resp =
    case resp of
        DisclaimerResp_ DisclaimerYes ->
            Disclaimer

        DisclaimerResp_ DisclaimerNo ->
            Disclaimer

        BFResp_ BFYes ->
            Disclaimer

        BFResp_ BFNo ->
            Disclaimer

        ITResp_ ITYes ->
            Disclaimer

        ITResp_ ITNo ->
            Disclaimer


copyFor : Lang -> View -> Html Msg
copyFor lang v =
    case lang of
        JP ->
            case v of
                Disclaimer ->
                    text ""

                BF ->
                    text ""

                IT ->
                    text ""

        EN ->
            case v of
                Disclaimer ->
                    text ""

                BF ->
                    text ""

                IT ->
                    text ""



-- agree tos
-- greeting
-- ? authentication
-- bf
-- it
--
-- POST Msg for logs


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            just model

        SetLang lang ->
            just { model | lang = lang }

        SetView view_ ->
            just { model | view = view_ }

        Post msg_ ->
            model |> cmds [ log msg_ ]


log : Msg -> Cmd Msg
log msg =
    let
        body =
            Http.stringBody "text/plain" (Debug.toString msg)

        handler =
            Http.expectString (always NoOp)
    in
    Http.post
        { url = rootPath
        , body = body
        , expect = handler
        }


rootPath : String
rootPath =
    Debug.todo "rootPath"



--- view ---


view : Model -> Html Msg
view model =
    text ""



-- viewDisclaimer : Html Msg
-- viewDisclaimer =
--- main ---


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--- helpers ---


just : Model -> ( Model, Cmd Msg )
just =
    cmds []


cmds : List (Cmd Msg) -> Model -> ( Model, Cmd Msg )
cmds cs model =
    ( model, Cmd.batch cs )
