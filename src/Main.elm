module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



--- init / subs ---

type alias Model =
  { foo : String
  }

init : Flags -> (Model, Cmd Msg)
init flags =
  cmds [] <| initModel flags

initModel : Flags -> Model
initModel flags =
  { foo = ""
  }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
  [
  ]


--- update ---

type Msg
  = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  NoOp ->
    just model



--- view ---

view : Model -> Html Msg
view model =
  text ""



--- main ---

type alias Flags = ()

main : Program Flags Model Msg
main = Browser.element
  { init          = init
  , update        = update
  , subscriptions = subscriptions
  , view          = view
  }



--- helpers ---

just : Model -> (Model, Cmd Msg)
just = cmds []

cmds : List (Cmd Msg) -> Model -> (Model, Cmd Msg)
cmds cs model = (model, Cmd.batch cs)
