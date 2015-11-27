--Animated branching fractals that grow to make you feel good.
--Be one with nature. Experience your physical form. Branches.

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time exposing (..)
import Signal exposing (..)
import Mouse exposing (..)


--MODEL (SOUTH)
type alias Model = {root : Position, length : Int, children: Children}

type Children = Children (List Model)

type alias Position = {x : Float, y : Float}

init : Model
init = { root = startingRoot, length = 1, children = Children [] }

startingRoot : Position
startingRoot =
  {x = 237, y = 42}

getChildren : Model -> List Model
getChildren model =
  extractModels model.children

extractModels : Children -> List Model
extractModels (Children children) =
  children

sproutFrom : Position -> Model
sproutFrom position =
  { root = position
  , length = 0
  , children = Children []
  }

--UPDATE (WEST)

type Action = NoOp
  | Grow
  | Shrink
  | Move Position
  | Branch Position

update : Action -> Model -> Model
update action model =
  case action of 
    NoOp -> model
    Grow -> grow model
    Move position -> move model position
    Branch position -> branch model position
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
  { model | children = oneMore ((getChildren model), position) }

oneMore : (List Model, Position) -> Children
oneMore (children, position) =
  Children (List.append children [sproutFrom position]) 

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
  Shrink

mouseAction : (Int, Int) -> Action
mouseAction (x, y) =
  Branch (toElmCoordinates {x = toFloat x, y = toFloat y})

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
    (model.root.x, (model.root.y + sizeOf model))

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
