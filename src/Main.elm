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
    , prev : View
    , lang : Lang
    , flash : Flash
    , cd : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    cmds [] <| initModel flags


initModel : Flags -> Model
initModel flags =
    { view = Disclaimer
    , prev = Disclaimer
    , lang = JP
    , flash = NoFlash
    , cd = False
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



--- update ---


type Msg
    = NoOp
    | Back
    | SetLang Lang
    | GotResp Resp


type View
    = Disclaimer
    | CD
    | SS
    | AK
    | KI
    | BF
    | IT
    | Win
    | Fail


type Flash
    = NoFlash
    | Flash (Lang -> Html Msg)


type DisclaimerResp
    = DisclaimerYes


type SSResp
    = SSYes
    | SSNo


type AKResp
    = AKM
    | AKG


type KIResp
    = KIYes
    | KINo


type CDResp
    = CDC
    | CDN


type BFResp
    = BFYes
    | BFNo


type ITResp
    = ITYes
    | ITNo


type Resp
    = DisclaimerResp_ DisclaimerResp
    | SSResp_ SSResp
    | AKResp_ AKResp
    | KIResp_ KIResp
    | CDResp_ CDResp
    | BFResp_ BFResp
    | ITResp_ ITResp


redirect : Resp -> ( View, Flash, Maybe Bool )
redirect resp =
    case resp of
        DisclaimerResp_ DisclaimerYes ->
            ( CD, NoFlash, Nothing )

        CDResp_ CDC ->
            ( SS, understoodFlash, Just True )

        CDResp_ CDN ->
            ( SS, understoodFlash, Just False )

        SSResp_ SSYes ->
            ( AK, NoFlash, Nothing )

        SSResp_ SSNo ->
            ( SS, errorDetectedFlash, Nothing )

        AKResp_ AKM ->
            ( KI, NoFlash, Nothing )

        AKResp_ AKG ->
            ( AK, errorDetectedFlash, Nothing )

        KIResp_ KIYes ->
            ( BF, NoFlash, Nothing )

        KIResp_ KINo ->
            ( KI, errorDetectedFlash, Nothing )

        BFResp_ BFYes ->
            ( Fail, NoFlash, Nothing )

        BFResp_ BFNo ->
            ( IT, NoFlash, Nothing )

        ITResp_ ITYes ->
            ( Win, NoFlash, Nothing )

        ITResp_ ITNo ->
            ( IT, errorDetectedFlash, Nothing )


understoodFlash : Flash
understoodFlash =
    Flash <|
        \lang ->
            case lang of
                JP ->
                    text ""

                EN ->
                    text ""


errorDetectedFlash : Flash
errorDetectedFlash =
    Flash <|
        \lang ->
            case lang of
                JP ->
                    text "err"

                EN ->
                    text "err"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            just model

        Back ->
            just { model | view = model.prev }

        SetLang lang ->
            just { model | lang = lang }

        GotResp resp ->
            let
                ( view_, flash, mCd ) =
                    redirect resp
            in
            { model
                | view = view_
                , prev = model.view
                , flash = flash
                , cd = Maybe.withDefault model.cd mCd
            }
                |> cmds [ log resp ]


log : Resp -> Cmd Msg
log resp =
    let
        body =
            Http.stringBody "text/plain" (Debug.toString resp)

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
    "http://localhost:5555"



--- view ---


view : Model -> Html Msg
view model =
    div []
        [ renderFlash model
        , copyFor model.lang model.view
        , backBtn model
        ]


renderFlash : Model -> Html Msg
renderFlash model =
    case model.flash of
        NoFlash ->
            text ""

        Flash f ->
            div []
                [ f model.lang
                ]


backBtn : Model -> Html Msg
backBtn model =
    let
        txt =
            case model.lang of
                EN ->
                    text "Back"

                JP ->
                    text "戻る"
    in
    a [ onClick Back ] [ txt ]


copyFor : Lang -> View -> Html Msg
copyFor lang v =
    case lang of
        JP ->
            case v of
                Disclaimer ->
                    div []
                        [ p [] [ text "Disclaimer" ]
                        , hr [] []
                        , p []
                            [ a [ onClick <| GotResp <| DisclaimerResp_ DisclaimerYes ]
                                [ text "承知"
                                ]
                            ]
                        ]

                CD ->
                    div []
                        [ p [] [ text "CD" ]
                        , hr [] []
                        , p []
                            [ a [ onClick <| GotResp <| CDResp_ CDC ]
                                [ text "CDC"
                                ]
                            ]
                        , p []
                            [ a [ onClick <| GotResp <| CDResp_ CDN ]
                                [ text "CDN"
                                ]
                            ]
                        ]

                SS ->
                    div []
                        [ p [] [ text "SS" ]
                        , hr [] []
                        , p []
                            [ a [ onClick <| GotResp <| SSResp_ SSYes ]
                                [ text "はい"
                                ]
                            ]
                        , p []
                            [ a [ onClick <| GotResp <| SSResp_ SSNo ]
                                [ text "いいえ"
                                ]
                            ]
                        ]

                AK ->
                    div []
                        [ p [] [ text "AK" ]
                        , hr [] []
                        , p []
                            [ a [ onClick <| GotResp <| AKResp_ AKM ]
                                [ text "AKM"
                                ]
                            ]
                        , p []
                            [ a [ onClick <| GotResp <| AKResp_ AKG ]
                                [ text "AKG"
                                ]
                            ]
                        ]

                KI ->
                    div []
                        [ p [] [ text "KI" ]
                        , hr [] []
                        , p []
                            [ a [ onClick <| GotResp <| KIResp_ KIYes ]
                                [ text "はい"
                                ]
                            ]
                        , p []
                            [ a [ onClick <| GotResp <| KIResp_ KINo ]
                                [ text "いいえ"
                                ]
                            ]
                        ]

                BF ->
                    div []
                        [ p [] [ text "BF" ]
                        , hr [] []
                        , p []
                            [ a [ onClick <| GotResp <| BFResp_ BFYes ]
                                [ text "はい"
                                ]
                            ]
                        , p []
                            [ a [ onClick <| GotResp <| BFResp_ BFNo ]
                                [ text "いいえ"
                                ]
                            ]
                        ]

                IT ->
                    div []
                        [ p [] [ text "IT" ]
                        , hr [] []
                        , p []
                            [ a [ onClick <| GotResp <| ITResp_ ITYes ]
                                [ text "はい"
                                ]
                            ]
                        , p []
                            [ a [ onClick <| GotResp <| ITResp_ ITNo ]
                                [ text "いいえ"
                                ]
                            ]
                        ]

                Win ->
                    div []
                        [ p [] [ text "Win" ]
                        ]

                Fail ->
                    div []
                        [ p [] [ text "Fail" ]
                        ]

        EN ->
            case v of
                Disclaimer ->
                    text ""

                SS ->
                    text ""

                AK ->
                    text ""

                KI ->
                    text ""

                CD ->
                    text ""

                BF ->
                    text ""

                IT ->
                    text ""

                Win ->
                    text ""

                Fail ->
                    text ""



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
