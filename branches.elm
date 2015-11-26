--Animated branching fractals that grow to make you feel good.
--Be one with nature. Experience your physical form. Branches.

module Branch (Model, init, Action, update, view) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time exposing (..)
import Signal exposing (..)
import Mouse exposing (..)


--MODEL (SOUTH)
type alias Model = {root : {x: Float, y : Float}, length : Int, children: Children}

type Children = Children (List Model)

init : Model
init = { root = {x = 237, y = 42}, length = 1, children = Children [] }

--UPDATE (WEST)

type Action = Grow
  | NoOp
  | Move {x : Float, y : Float}
  | Branch

update : Action -> Model -> Model
update action model =
  case action of 
    NoOp -> model
    Grow -> grow model
    Move position -> move model position
    Branch -> branch model


grow : Model -> Model
grow model =
  { model | length = model.length + 1 }

move : Model -> {x: Float, y: Float} -> Model
move model position =
  { model | root = toElmCoordinates position }

toElmCoordinates : {x : Float, y : Float} -> {x : Float, y : Float}
toElmCoordinates mouse =
  {x = mouse.x - toFloat width / 2, y = toFloat height / 2 - mouse.y}

branch : Model -> Model
branch model =
  { model | root = {x = model.root.x / 2, y = model.root.y / 2}}


actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox NoOp

address : Address Action
address =
  actionMailbox.address

actions : Signal Action
actions =
  Signal.merge
    fastSignal slowSignal


fastSignal : Signal Action
fastSignal =
  Signal.map fastAction (fps 42)

slowSignal : Signal Action
slowSignal =
  Signal.map slowAction (Time.every (3 * Time.second))


fastAction : Time -> Action
fastAction frame =
  Grow

slowAction : Time -> Action
slowAction second =
  Branch


--VIEW (NORTH)

view : Address Action -> Model -> Element
view address model =
  collage width height [incarnate model]

incarnate : Model -> Form
incarnate model =
  trunkPath model
  |> draw model

draw : Model -> Path -> Form
draw model =
  colorOf model |> solid |> traced

trunkPath : Model -> Path
trunkPath model =
  segment 
    (model.root.x, model.root.y) 
    (model.root.x, (model.root.y + sizeOf model))

colorOf : Model -> Color
colorOf model =
  rgb 
    (1)
    (66)
    (42)

sizeOf : Model -> Float
sizeOf model =
  toFloat model.length / 2


--MAIN (EAST)

main : Signal Element
main =
  Signal.map (view address)
    (Signal.foldp update init actions)

width : Int
width = 666

height : Int
height = 713




--(UNTIL THE EAST SHALL RISE)
--(AND ILLUMINATE ALL BEINGS)
