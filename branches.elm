--Animated branching fractals that grow to make you feel good.
--Be one with nature. Experience your physical form. Branches.

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time exposing (..)
import Signal exposing (..)
import Mouse exposing (..)

--MODEL (SOUTH)
type alias Model = {root : {x: Float, y : Float}, length : Int}

init : Model
init = { root = {x = 1, y = 1}, length = 1 }

--UPDATE (WEST)

type Action = Grow
  | NoOp
  | Move {x : Float, y : Float}

update : Action -> Model -> Model
update action model =
  case action of 
    NoOp -> model
    Grow -> { model | length = model.length + 1 }
    Move position -> { model | root = transpose position }

transpose : {x : Float, y : Float} -> {x : Float, y : Float}
transpose mouse =
  {x = mouse.x - toFloat width / 2, y = toFloat height / 2 - mouse.y}

width : Int
width = 1372

height : Int
height = 713

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox NoOp

address : Address Action
address =
  actionMailbox.address

actions : Signal Action
actions =
  Signal.map timeMapper (fps 42)

timeMapper : Time -> Action
timeMapper frame =
  Grow


--VIEW (NORTH)

view : Address Action -> Model -> Element
view address model =
  collage width height [trunk model]

trunk : Model -> Form
trunk model =
  segment (model.root.x, model.root.y) (model.root.x, (model.root.y + sizeOf model))
    |> traced (solid (colorOf model))

colorOf : Model -> Color
colorOf model =
  rgb 
    (1)
    (66)
    (42)

sizeOf : Model -> Float
sizeOf model =
  toFloat model.length / 5


--MAIN (EAST)

main : Signal Element
main =
  Signal.map (view address)
    (Signal.foldp update init actions)




--(UNTIL THE EAST SHALL RISE)
--(AND ILLUMINATE ALL BEINGS)
