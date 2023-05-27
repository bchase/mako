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
    , n : N
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
    , n = Namae
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
    | Flash String (Lang -> Html Msg)


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
    | CDSan
    | CDSama
    | CDKakka


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


redirect : N -> Resp -> ( View, Flash, Maybe N )
redirect n resp =
    case resp of
        DisclaimerResp_ DisclaimerYes ->
            ( CD, NoFlash, Nothing )

        CDResp_ CDC ->
            ( SS, understoodFlash, Just Chan )

        CDResp_ CDN ->
            ( SS, understoodFlash, Just Namae )

        CDResp_ CDSan ->
            ( SS, understoodFlash, Just San )

        CDResp_ CDSama ->
            ( SS, understoodFlash, Just Sama )

        CDResp_ CDKakka ->
            ( SS, understoodFlash, Just Kakka )

        SSResp_ SSYes ->
            ( AK, NoFlash, Nothing )

        SSResp_ SSNo ->
            ( SS, autheErrorFlash n, Nothing )

        AKResp_ AKM ->
            ( KI, NoFlash, Nothing )

        AKResp_ AKG ->
            ( AK, autheErrorFlash n, Nothing )

        KIResp_ KIYes ->
            -- ( BF, NoFlash, Nothing )
            ( IT, NoFlash, Nothing )

        KIResp_ KINo ->
            ( KI, autheErrorFlash n, Nothing )

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
    Flash "green" <|
        \lang ->
            case lang of
                JP ->
                    text "畏まりました"

                EN ->
                    text ""


autheErrorFlash : N -> Flash
autheErrorFlash n =
    Flash "red" <|
        \lang ->
            case lang of
                JP ->
                    text <| name n ++ "じゃなさそうです。もう一度お試しください。"

                EN ->
                    text "authe"


errorDetectedFlash : Flash
errorDetectedFlash =
    Flash "red" <|
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
                ( view_, flash, mN ) =
                    redirect model.n resp
            in
            { model
                | view = view_
                , past = view_ :: model.past
                , flash = flash
                , n = Maybe.withDefault model.n mN
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
    "https://mako-app.herokuapp.com"



--- view ---


view : Model -> Html Msg
view model =
    div
        [ class "grid h-screen place-items-center text-5xl p-10"
        ]
        ([ renderFlash model ]
            ++ contentFor model
            ++ [ hr_
               , backBtn model
               ]
        )


renderFlash : Model -> Html Msg
renderFlash model =
    case model.flash of
        NoFlash ->
            text ""

        Flash color f ->
            div
                [ class <| "bg-" ++ color ++ "-200 text-" ++ color ++ "-900 rounded-lg shadow-md p-6 pr-10"
                ]
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
    btn "sky" (GotResp resp) txt


btn : String -> Msg -> String -> Html Msg
btn color msg txt =
    button
        [ type_ "button"
        , class <| "text-" ++ color ++ "-500 border border-" ++ color ++ "-500 hover:bg-" ++ color ++ "-500 hover:text-white active:bg-" ++ color ++ "-600 font-bold uppercase px-6 py-3 rounded outline-none focus:outline-none mr-1 mb-1 ease-linear transition-all duration-150 w-2/5"
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


disclaimerCopy : List String
disclaimerCopy =
    [ "マコ・アップへようこそ！"
    , "開発がかっこいいと言われてくれたブラッドはこのちょっとしたバカバカしいアプリを作ってしまいました。"
    , "ブラッドは日本語下手で、このアプリをめちゃくちゃ適当に作っちゃったから、そのようなものについて問題や過ちがあったら、ご理解とご協力をお願いいたします。"
    ]


cdCopy : List String
cdCopy =
    [ "ところで、ブラッドにはまだよく分からない日本語たくさんありますね。"
    , "例えば、ちゃん付けのほうがなれなれしい？それとも名前だけ？という問題ですね。"
    , "ですから、ブラッドからの呼び方を選んでください"
    ]


ssCopy : N -> List String
ssCopy n =
    [ "では、今このアプリを使っていらっしゃる者は本当に" ++ name n ++ "であることを確認しないといけません。"
    , "いくつか質問を答えてください。"
    , "ご出身は京都でございますか？"
    ]


akCopy : N -> List String
akCopy n =
    [ name n ++ "は四個のアイスの実を召し上がったらどんな味でしょうか？"
    ]


kiCopy : N -> List String
kiCopy n =
    [ name n ++ "の笑顔と笑いがめちゃくちゃかわいいですか？"
    ]


itCopy : N -> List String
itCopy n =
    [ "確かに" ++ name n ++ "でございますね。"
    , "実はこのアプリの目的は簡単です。ブラッドの一つの質問の答えを" ++ name n ++ "から頂くことです。"
    , "ブラッドと一緒に食事とか散歩とかハイキングとか行きたいでございますか？"
    ]


winCopy : N -> List String
winCopy n =
    [ "やった！！……あ、ちゃう。畏まりました。"
    , "ブラッドに通知いたします。"
    , "マコ・アップを使ってくれてありがとうございました。"
    , "以上です。"
    ]


type N
    = Chan
    | Namae
    | San
    | Sama
    | Kakka


name : N -> String
name n =
    case n of
        Chan ->
            "まこちゃん"

        Namae ->
            "まこ"

        San ->
            "まこさん"

        Sama ->
            "まこ様"

        Kakka ->
            "まこ閣下"


renderCopy : List String -> List (Html Msg)
renderCopy =
    List.map (\line -> p [] [ text line ])


contentFor : Model -> List (Html Msg)
contentFor model =
    let
        lang =
            model.lang

        v =
            model.view

        ( copy, btns ) =
            case lang of
                JP ->
                    case v of
                        Disclaimer ->
                            ( disclaimerCopy
                            , [ btn_ (DisclaimerResp_ DisclaimerYes) "承知"
                              ]
                            )

                        CD ->
                            ( cdCopy
                            , [ btn_ (CDResp_ CDC) "まこちゃん"
                              , btn_ (CDResp_ CDN) "まこ"
                              , btn_ (CDResp_ CDSan) "まこさん"
                              , btn_ (CDResp_ CDSama) "まこ様"
                              , btn_ (CDResp_ CDKakka) "まこ閣下"
                              ]
                            )

                        SS ->
                            ( ssCopy model.n
                            , [ btn_ (SSResp_ SSYes) "はい"
                              , btn_ (SSResp_ SSNo) "いいえ"
                              ]
                            )

                        AK ->
                            ( akCopy model.n
                            , [ btn_ (AKResp_ AKM) "マンゴ"
                              , btn_ (AKResp_ AKG) "葡萄"
                              ]
                            )

                        KI ->
                            ( kiCopy model.n
                            , [ btn_ (KIResp_ KIYes) "はい"
                              , btn_ (KIResp_ KINo) "いいえ"
                              ]
                            )

                        BF ->
                            ( [ "--" ]
                              -- ( [ "BF" ]
                            , [ btn_ (BFResp_ BFYes) "はい"
                              , btn_ (BFResp_ BFNo) "いいえ"
                              ]
                            )

                        IT ->
                            ( itCopy model.n
                            , [ btn_ (ITResp_ ITYes) "はい"
                              , btn_ (ITResp_ ITNo) "いいえ"
                              ]
                            )

                        Win ->
                            ( winCopy model.n
                            , []
                            )

                        Fail ->
                            ( [ "Fail" ]
                            , []
                            )

                EN ->
                    -- case v of
                    --     Disclaimer ->
                    --         []
                    --     SS ->
                    --         []
                    --     AK ->
                    --         []
                    --     KI ->
                    --         []
                    --     CD ->
                    --         []
                    --     BF ->
                    --         []
                    --     IT ->
                    --         []
                    --     Win ->
                    --         []
                    --     Fail ->
                    --         []
                    ( [], [] )
    in
    renderCopy copy ++ [ hr_ ] ++ btns



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
