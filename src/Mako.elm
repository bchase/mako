module Mako exposing (main)

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
    , past : List View
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
    , past = []
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
            let
                prev =
                    model.past
                        |> List.head
                        |> Maybe.withDefault Disclaimer

                past =
                    model.past
                        |> List.tail
                        |> Maybe.withDefault []
            in
            just { model | view = prev, past = past }

        SetLang lang ->
            just { model | lang = lang }

        GotResp resp ->
            let
                ( view_, flash, mCd ) =
                    redirect resp
            in
            { model
                | view = view_
                , past = view_ :: model.past
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
    div
        [ class "grid h-screen place-items-center"
        ]
        ([ renderFlash model ]
            ++ contentFor model.lang model.view
            ++ [ hr_
               , backBtn model
               ]
        )


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
                    "Back"

                JP ->
                    "戻る"
    in
    btn "black" Back txt


btn_ : Resp -> String -> Html Msg
btn_ resp txt =
    btn "rose" (GotResp resp) txt


btn : String -> Msg -> String -> Html Msg
btn color msg txt =
    button
        [ type_ "button"
        , class <| "text-" ++ color ++ "-500 border border-" ++ color ++ "-500 hover:bg-" ++ color ++ "-500 hover:text-white active:bg-" ++ color ++ "-600 font-bold uppercase text-sm px-6 py-3 rounded outline-none focus:outline-none mr-1 mb-1 ease-linear transition-all duration-150 w-2/5"
        , onClick msg
        ]
        [ text txt
        ]


hr_ : Html Msg
hr_ =
    div []
        [ br [] []
        , hr [] []
        , br [] []
        ]


contentFor : Lang -> View -> List (Html Msg)
contentFor lang v =
    case lang of
        JP ->
            case v of
                Disclaimer ->
                    [ p [] [ text "Disclaimer" ]
                    , hr_
                    , btn_ (DisclaimerResp_ DisclaimerYes) "承知"
                    ]

                CD ->
                    [ p [] [ text "CD" ]
                    , hr_
                    , btn_ (CDResp_ CDC) "CDC"
                    , btn_ (CDResp_ CDN) "CDN"
                    ]

                SS ->
                    [ p [] [ text "SS" ]
                    , hr_
                    , btn_ (SSResp_ SSYes) "はい"
                    , btn_ (SSResp_ SSNo) "いいえ"
                    ]

                AK ->
                    [ p [] [ text "AK" ]
                    , hr_
                    , btn_ (AKResp_ AKM) "AKM"
                    , btn_ (AKResp_ AKG) "AKG"
                    ]

                KI ->
                    [ p [] [ text "KI" ]
                    , hr_
                    , btn_ (KIResp_ KIYes) "はい"
                    , btn_ (KIResp_ KINo) "いいえ"
                    ]

                BF ->
                    [ p [] [ text "BF" ]
                    , hr_
                    , btn_ (BFResp_ BFYes) "はい"
                    , btn_ (BFResp_ BFNo) "いいえ"
                    ]

                IT ->
                    [ p [] [ text "IT" ]
                    , hr_
                    , btn_ (ITResp_ ITYes) "はい"
                    , btn_ (ITResp_ ITNo) "いいえ"
                    ]

                Win ->
                    [ p [] [ text "Win" ]
                    ]

                Fail ->
                    [ p [] [ text "Fail" ]
                    ]

        EN ->
            case v of
                Disclaimer ->
                    []

                SS ->
                    []

                AK ->
                    []

                KI ->
                    []

                CD ->
                    []

                BF ->
                    []

                IT ->
                    []

                Win ->
                    []

                Fail ->
                    []



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
