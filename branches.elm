--Animated branching fractals that grow to make you feel good.
--Be one with nature. Experience your physical form. Branches.

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time exposing (..)
import Signal exposing (..)
import Mouse exposing (..)

--MODEL (SOUTH)
type alias Model = Int

init : Model
init = 1

--UPDATE (WEST)

type Action = Tick 
  | Tock
  | NoOp

update : Action -> Model -> Model
update action model =
  model + 1 

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox NoOp

address : Address Action
address =
  actionMailbox.address

actions : Signal Action
actions =
  actionMailbox.signal

--VIEW (NORTH)

view : Address Action -> Model -> Element
view address model =
  collage 271 137 [filled  (colorOf model) (circle (sizeOf model))]

colorOf : Model -> Color
colorOf model =
  rgb 
    (255 - model)
    (floor (255 / toFloat model))
    (255 % model)

sizeOf : Model -> Float
sizeOf model =
  toFloat model * 73.1


--MAIN (EAST)

main : Signal Element
main =
  Signal.map (view address)
    (Signal.foldp update init actions) 


--(UNTIL THE EAST SHALL RISE)
--(AND ILLUMINATE ALL BEINGS)
