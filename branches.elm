--Animated branching fractals that grow to make you feel good.
--Be one with nature. Experience your physical form. Branches.

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time exposing (..)
import Signal exposing (..)
import Mouse exposing (..)


--MODEL (SOUTH)
type alias Model = 
  { root : Position
  , length : Int
  , maxLength : Int
  , children : Children
  , sproutingLeft : Bool
  , angle : Radians
  }

type Children = Children (List Model)

type alias Position = {x : Float, y : Float}
type alias Radians = Float
type alias Degrees = Float

init : Model
init = 
  { root = startingRoot
  , length = 1
  , maxLength = 127
  , children = Children []
  , sproutingLeft = True
  , angle = pi/2
  }

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
  , maxLength = parent.maxLength - 10
  , children = Children []
  , sproutingLeft = not parent.sproutingLeft
  , angle = angleToSproutFrom parent
  }

angleToSproutFrom : Model -> Radians
angleToSproutFrom parent =
  if parent.sproutingLeft then
    parent.angle + pi/6.5
  else
    parent.angle - pi/6.5

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
    Move position -> move position model
    Branch -> branch model
    Shrink -> shrink model


grow : Model -> Model
grow model =
  if model.length < model.maxLength then
    { model | length = model.length + 1, children = applyToChildren grow model }
  else
    { model | children = applyToChildren grow model }

shrink : Model -> Model
shrink model =
  { model | length = model.length * 6 // 7, children = applyToChildren shrink model }

applyToChildren : (Model -> Model) -> Model -> Children
applyToChildren applicator model =
  Children (List.map applicator (getChildren model))

move : Position -> Model -> Model
move position model =
  { model | root = toElmCoordinates position, children = moveChildren position model }

moveChildren : Position -> Model -> Children
moveChildren position model =
  applyToChildren (moveOffset model position) model

moveOffset : Model -> Position -> Model -> Model
moveOffset parent position child =
  move 
  { x = child.root.x - parent.root.x + position.x
  , y = parent.root.y - child.root.y + position.y
  } child

toElmCoordinates : Position -> Position
toElmCoordinates mouse =
  {x = mouse.x - toFloat width / 2, y = toFloat height / 2 - mouse.y}

branch : Model -> Model
branch model =
  if model.length < model.maxLength then
    { model | children = branchedChildren model }
  else
    proudParent model

branchedChildren : Model -> Children
branchedChildren model =
  proudParent model |>
  oneMore (tip model)

proudParent : Model -> Model
proudParent model =
  { model | children = applyToChildren branch model }

oneMore : Position -> Model -> Children
oneMore position model =
  Children (List.append (getChildren model) [sproutFrom position model]) 

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox NoOp

address : Address Action
address =
  actionMailbox.address

actions : Signal Action
actions =
  Signal.merge mouseSignal
    (Signal.merge slowSignal fastSignal)


fastSignal : Signal Action
fastSignal =
  Signal.map fastAction (fps 42)

slowSignal : Signal Action
slowSignal =
  Signal.map slowAction (Time.every (Time.minute / 72))

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
  collage width height (incarnate model)

incarnate : Model -> List Form
incarnate model =
  List.append [drawTrunk model] (incarnateChildren model)

drawTrunk : Model -> Form
drawTrunk model =
  trunkPath model
    |> draw model

incarnateChildren : Model -> List Form
incarnateChildren model =
  List.map incarnate (getChildren model)
    |> List.concat

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
