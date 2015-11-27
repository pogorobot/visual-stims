--Animated branching fractals that grow to make you feel good.
--Be one with nature. Experience your physical form. Branches.

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time exposing (..)
import Signal exposing (..)
import Mouse exposing (..)


--MODEL (SOUTH)
type alias Model = {root : Position, length : Int, children : Children, angle : Radians}

type Children = Children (List Model)

type alias Position = {x : Float, y : Float}
type alias Radians = Float
type alias Degrees = Float

init : Model
init = { root = startingRoot, length = 1, children = Children [], angle = pi/2 }

startingRoot : Position
startingRoot =
  {x = 237, y = 42}

direction : Model -> Position
direction model =
  {x = cos model.angle, y = sin model.angle}

getChildren : Model -> List Model
getChildren model =
  extractModels model.children

extractModels : Children -> List Model
extractModels (Children children) =
  children

sproutFrom : Position -> Model -> Model
sproutFrom position parent =
  { root = position
  , length = 0
  , children = Children []
  , angle = parent.angle + pi/6.5
  }

--UPDATE (WEST)

type Action = NoOp
  | Grow
  | Shrink
  | Move Position
  | Branch

update : Action -> Model -> Model
update action model =
  case action of 
    NoOp -> model
    Grow -> grow model
    Move position -> move model position
    Branch -> branch model (tip model)
    Shrink -> shrink model


grow : Model -> Model
grow model =
  { model | length = model.length + 1, children = applyToChildren grow model }

shrink : Model -> Model
shrink model =
  { model | length = model.length * 6 // 7, children = applyToChildren shrink model }

applyToChildren : (Model -> Model) -> Model -> Children
applyToChildren applicator model =
  Children (List.map applicator (getChildren model))

move : Model -> Position -> Model
move model position =
  { model | root = toElmCoordinates position }

toElmCoordinates : Position -> Position
toElmCoordinates mouse =
  {x = mouse.x - toFloat width / 2, y = toFloat height / 2 - mouse.y}

branch : Model -> Position -> Model
branch model position =
  { model | children = oneMore (model, position) }

oneMore : (Model, Position) -> Children
oneMore (model, position) =
  Children (List.append (getChildren model) [sproutFrom position model]) 

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox NoOp

address : Address Action
address =
  actionMailbox.address

actions : Signal Action
actions =
  Signal.merge slowSignal
  (Signal.merge
    fastSignal mouseSignal)


fastSignal : Signal Action
fastSignal =
  Signal.map fastAction (fps 42)

slowSignal : Signal Action
slowSignal =
  Signal.map slowAction (Time.every (Time.minute / 63))

mouseSignal : Signal Action
mouseSignal =
  Signal.map mouseAction (Mouse.position)




fastAction : Time -> Action
fastAction frame =
  Grow

slowAction : Time -> Action
slowAction second =
  Branch

mouseAction : (Int, Int) -> Action
mouseAction (x, y) =
  Move ( {x = toFloat x, y = toFloat y})

--VIEW (NORTH)

view : Address Action -> Model -> Element
view address model =
  collage width height (List.append [incarnate model] (List.map incarnate (getChildren model)))

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
    (tip model  |> toScreenCoordinates)

tip : Model -> Position
tip model =
  { x = model.root.x + (sizeOf model * cos model.angle)
  , y = model.root.y + (sizeOf model * sin model.angle)
  }

toScreenCoordinates : Position -> (Float, Float)
toScreenCoordinates position =
  ( position.x
  , position.y
  )


colorOf : Model -> Color
colorOf model =
  rgb 
    (1)
    (66)
    (42)

sizeOf : Model -> Float
sizeOf model =
  toFloat model.length


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
