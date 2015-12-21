--Animated branching fractals that grow to make you feel good.
--Be one with nature. Experience your physical form. Branches.

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time exposing (..)
import Signal exposing (..)
import Mouse exposing (..)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


--MODEL (SOUTH)
type alias Tree = 
  { root : Position
  , length : Int
  , maxLength : Int
  , branches : Branches
  , sproutingLeft : Bool
  , angle : Radians
  }

type Branches = Branches (List Tree)

type alias Position = {x : Float, y : Float}
type alias Radians = Float
type alias Degrees = Float

init : Tree
init = 
  { root = startingRoot
  , length = 1
  , maxLength = 127
  , branches = Branches []
  , sproutingLeft = True
  , angle = pi/2
  }

startingRoot : Position
startingRoot =
  {x = 0, y = -(toFloat height) / 2}

direction : Tree -> Position
direction model =
  {x = cos model.angle, y = sin model.angle}

getBranches : Tree -> List Tree
getBranches model =
  extractTrees model.branches

extractTrees : Branches -> List Tree
extractTrees (Branches branches) =
  branches

sproutFrom : Position -> Tree -> Tree
sproutFrom position parent =
  { root = position
  , length = 0
  , maxLength = parent.maxLength - 10
  , branches = Branches []
  , sproutingLeft = not parent.sproutingLeft
  , angle = angleToSproutFrom parent
  }

angleToSproutFrom : Tree -> Radians
angleToSproutFrom parent =
  if parent.sproutingLeft then
    parent.angle + pi/6.5
  else
    parent.angle - pi/6.5

--UPDATE (WEST)

type Action = NoOp
  | Grow
  | Branch
  | GrowTowards Position

update : Action -> Tree -> Tree
update action model =
  case action of 
    NoOp -> model
    Grow -> grow model
    Branch -> branch model
    GrowTowards position -> growTowards position model

grow : Tree -> Tree
grow model =
  if (canGrow model) then
    { model | length = grown model.length, branches = applyToBranches grow model }
  else
    { model | branches = applyToBranches grow model }

grown : Int -> Int
grown length =
  length + 1

canGrow : Tree -> Bool
canGrow model =
  model.length < model.maxLength

growTowards : Position -> Tree -> Tree
growTowards sun tree =
  if (canGrow tree) then
    rotate sun tree
  else
    { tree | branches = applyToBranches (growTowards sun) tree }

shrink : Tree -> Tree
shrink model =
  if (canShrink model) then
    { model | length = shrunk model.length, branches = applyToBranches shrink model }
  else
    { model | branches = applyToBranches shrink model }

shrunk : Int -> Int
shrunk length =
  length * 6 // 7

canShrink : Tree -> Bool
canShrink model =
  True

applyToBranches : (Tree -> Tree) -> Tree -> Branches
applyToBranches applicator model =
  Branches (List.map applicator (getBranches model))

move : Position -> Tree -> Tree
move position model =
  { model | root = toElmCoordinates position, branches = moveBranches position model }

moveBranches : Position -> Tree -> Branches
moveBranches position model =
  applyToBranches (moveOffset model position) model

moveOffset : Tree -> Position -> Tree -> Tree
moveOffset parent position child =
  move 
  { x = child.root.x - parent.root.x + position.x
  , y = parent.root.y - child.root.y + position.y
  } child

toElmCoordinates : Position -> Position
toElmCoordinates mouse =
  {x = mouse.x - toFloat width / 2, y = toFloat height / 2 - mouse.y}

branch : Tree -> Tree
branch model =
  if model.length < model.maxLength then
    { model | branches = branchedBranches model, sproutingLeft = not model.sproutingLeft }
  else
    proudParent model

branchedBranches : Tree -> Branches
branchedBranches model =
  proudParent model |>
  oneMore (tip model)

proudParent : Tree -> Tree
proudParent model =
  { model | branches = applyToBranches branch model }

oneMore : Position -> Tree -> Branches
oneMore position model =
  Branches (List.append (getBranches model) [sproutFrom position model]) 

rotate : Position -> Tree -> Tree
rotate position model =
  { model | angle = rotated position model, branches = applyToBranches (rotateChildOf model position) model}

rotated : Position -> Tree -> Radians
rotated position model = 
  (atan2 (position.y - model.root.y) (position.x - model.root.x))

rotateChildOf : Tree -> Position -> Tree -> Tree
rotateChildOf parent position child =
  { child | angle = child.angle - parent.angle + (rotated position parent)
  , branches = applyToBranches (rotateChildOf parent position) child
  }


accelerate : Position -> Tree -> Tree
accelerate direction model =
  model

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
  GrowTowards ( toElmCoordinates {x = toFloat x, y = toFloat y})

--VIEW (NORTH)

view : Address Action -> Tree -> Html
view address model =
  Svg.svg 
    [ Svg.Attributes.height (toString height), Svg.Attributes.width (toString width)]--, Svg.Attributes.viewBox "0 0 2000 2000"]
    ( incarnate model )

incarnate : Tree -> List Svg
incarnate model =
  List.append [shapeOf model] (incarnateBranches model)

incarnateBranches : Tree -> List Svg
incarnateBranches model =
  List.map incarnate (getBranches model)
    |> List.concat

shapeOf : Tree -> Svg
shapeOf model =
  Svg.path 
    [d (svgPath model), stroke "green", strokeWidth "1", fill "none"]
    []

svgPath : Tree -> String
svgPath model =
  "M"
    ++ (model.root |> svgFormat)
    ++ " Q" 
    ++ (controlPoint model |> svgFormat)
    ++ " " ++ (tip model |> svgFormat)

controlPoint : Tree -> Position
controlPoint model = 
  { x = ((model.root.x + (tip model).x) / 2) + ((sizeOf model / 10) * cos model.angle)
  , y = ((model.root.y + (tip model).y) / 2) - ((sizeOf model / 10) * sin model.angle) 
  }

svgFormat : Position -> String
svgFormat position =
  toString (toHtmlCoordinates position).x ++ "," ++ toString (toHtmlCoordinates position).y

tip : Tree -> Position
tip model =
  { x = model.root.x + (sizeOf model * cos model.angle)
  , y = model.root.y + (sizeOf model * sin model.angle)
  }

toScreenCoordinates : Position -> (Float, Float)
toScreenCoordinates position =
  ( position.x
  , position.y
  )

toHtmlCoordinates : Position -> Position
toHtmlCoordinates position =
  { x = position.x + toFloat width / 2
  , y = toFloat height / 2 - position.y
  }

svgColor : Color -> String
svgColor color =
  svgColor' (toRgb color)

svgColor' : { red: Int, green : Int, blue: Int, alpha : Float } -> String
svgColor' color =
  "#" ++ toHex color.red ++ toHex color.green ++ toHex color.blue

toHex : Int -> String
toHex number =
  toString (digitize (number // 16))
  ++ toString (digitize (number % 16))

digitize : Int -> String
digitize number =
  if number < 10 then
    toString number
  else
    Maybe.withDefault "X" (List.head (List.drop (number - 10) ["A", "B", "C", "D", "E", "F"]))

colorOf : Tree -> Color
colorOf model =
  rgb 
    (1)
    (66)
    (42)

sizeOf : Tree -> Float
sizeOf model =
  toFloat model.length

--MAIN (EAST)

main : Signal Html
main =
  Signal.map (view address)
    (Signal.foldp update init actions)

width : Int
width = 1000

height : Int
height = 800

--(UNTIL THE EAST SHALL RISE)
--(AND ILLUMINATE ALL BEINGS)
