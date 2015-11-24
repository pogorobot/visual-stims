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

type Action = Grow
  | NoOp

update : Action -> Model -> Model
update action model =
  case action of 
    NoOp -> model
    Grow -> model + 1

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox NoOp

address : Address Action
address =
  actionMailbox.address

actions : Signal Action
actions =
  Signal.map mouseMapper Mouse.x

mouseMapper : Int -> Action
mouseMapper x =
  Grow 


--VIEW (NORTH)

view : Address Action -> Model -> Element
view address model =
  collage 2371 1337 [filled  (colorOf model) (circle (sizeOf model))]

colorOf : Model -> Color
colorOf model =
  rgb 
    (255 - model)
    (floor (255 / toFloat model))
    (255 % model)

sizeOf : Model -> Float
sizeOf model =
  toFloat model / 2


--MAIN (EAST)

main : Signal Element
main =
  Signal.map (view address)
    (Signal.foldp update init actions)




--(UNTIL THE EAST SHALL RISE)
--(AND ILLUMINATE ALL BEINGS)
