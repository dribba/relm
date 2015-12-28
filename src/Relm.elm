module Relm where

import Color exposing (..)
import Text
import Time exposing (..)
import Keyboard
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type Scene 
  = Intro 
  | Stage Int
   
type Direction 
  = N
  | NW
  | W 
  | SW
  | S
  | SE
  | E
  | NE

type alias Input =
  { arrows: Velocity
  , time: Time
  }

type alias Position = 
  { x: Int
  , y: Int
  }
  
type alias Boundaries =
  { low: Int
  , high: Int
  }
  
type alias Velocity =
  { x: Int
  , y: Int
  }

type alias Character =
  { position: Position 
  , vx: Int
  , vy: Int
  , direction: Direction
  , angle: Int
  }

type alias State = 
  { scene: Scene
  , character: Character
  }


tileHeight = 40
tileWidth = 40

gameWidth = 480
gameHeight = 400

initialState = 
  { scene = Intro
  , character = 
    { position = 
      { x = 50
      , y = 50
      }
    , vx = 0
    , vy = 0
    , direction = S
    , angle = 180
    }
  }


-- VIEW
groundSprites : String
groundSprites = "img/ground.png"

grass : Element
grass = 
  croppedImage (240, 0) tileWidth tileHeight groundSprites


gameStats : State -> Element
gameStats state =
  "vx: " ++ (toString state.character.vx) ++ 
  " vy: " ++ (toString state.character.vy)  ++ 
  " dir: " ++ (toString state.character.direction) ++
  " ang: " ++ (toString state.character.angle) 
  |> Text.fromString
  |> leftAligned

gameContainer : Element -> Element
gameContainer =
  container gameWidth gameHeight middle

directionToAngle : Direction -> Int
directionToAngle direction =
  case direction of
    NW -> 315
    W -> 270
    SW -> 225
    S -> 180
    SE -> 135
    E -> 90
    NE -> 45
    N -> 0  

angleToDirection : Int -> Direction
angleToDirection angle =
  if angle == 0 then N
  else if angle == 45 then NE
  else if angle == 90 then E
  else if angle == 135 then SE
  else if angle == 180 then S
  else if angle == -135 then SW
  else if angle == -90 then W
  else if angle == -45 then NW
  else S 

directional rad = 
  let
  half = rad / 2
  in 
  filled green (polygon [ (0, rad)
              , (-half, -half)
              , (0, 0)
              , (half, -half)
              , (0, rad)
              ]
         )


direction : Character -> Form
direction character = 
  let 
    triangle = directional 10
  in
    triangle |> (character.angle |> toFloat |> negate |> degrees |> rotate)

mainCharacter : Character -> Form
mainCharacter character =
  group [ rect 20 20 
        |> filled red
        |> move (toFloat character.position.x, toFloat character.position.y)
        
      , direction character 
        |> move (toFloat character.position.x, toFloat character.position.y)
      ]    

printStats : Element -> Form
printStats stats =
  toForm(stats) |> move (50 - gameWidth / 2, 20 - gameHeight / 2)

gridSize = (12, 10)

createAndPlace : Int -> Int -> Element -> Form
createAndPlace x y tile =
  let 
    posX = ((toFloat x) * tileWidth + (tileWidth / 2) - (gameWidth / 2))
    posY = ((toFloat y) * tileHeight  + (tileHeight / 2) - (gameHeight / 2))
  in
    toForm(tile) |> move (posX, posY)

grid : (Int, Int) -> Element -> List Form
grid (w, h) tile = List.concatMap (\ix -> List.map (\iy -> createAndPlace ix iy tile) [0..(h-1)])  [0..(w-1)] 

stage : Element
stage =
  collage gameWidth gameHeight (grid gridSize grass)
  
view : State -> Element
view state =
  gameContainer <|
  collage gameWidth gameHeight
    [ toForm stage
    , printStats (gameStats state)
    , mainCharacter state.character
    ]

boundaries : Int -> Boundaries
boundaries size =
  let 
    half = size |> toFloat |> (\s -> s / 2)
  in
    { low = floor -half
    , high = ceiling half
    }
    
-- UPDATE
updateCoordinate : Boundaries -> Time -> Int -> Int -> Int
updateCoordinate boundaries time coord velocity =
  let 
    newCoord = (toFloat coord) + (toFloat velocity) * time
  in 
    clamp boundaries.low boundaries.high (round newCoord)

toDegrees rad =
  rad * 180 / pi

updatePosition : Input -> Character -> Character
updatePosition input character =
  let 
    position = character.position
    newPosition = { position |
              x = updateCoordinate (boundaries gameWidth) input.time position.x input.arrows.x,
              y = updateCoordinate (boundaries gameHeight) input.time position.y input.arrows.y 
            }
    angle = if input.arrows.x == 0 && input.arrows.y == 0 then 
          character.angle
        else 
          (atan2 (toFloat input.arrows.x) (toFloat input.arrows.y)) |> toDegrees |> round
    direction = angleToDirection angle 
   in
    { character | 
      position = newPosition,
      direction = direction,
      angle = angle
    }

updateVelocity : Input -> Character -> Character
updateVelocity input character =
  { character | 
    vx = input.arrows.x,
    vy = input.arrows.y
  }


update : Input -> State -> State
update input state =
  { state | 
    character = state.character 
            |> updateVelocity input
            |> updatePosition input
  }


-- MAIN
main : Signal Element
main =
  Signal.map view (Signal.foldp update initialState input)


-- KEYS
input : Signal Input
input = 
  Signal.sampleOn delta (Signal.map2 (\time arrows -> { time = time, arrows = arrows }) delta Keyboard.arrows)

delta : Signal Time  
delta =
  Signal.map (\t -> t / 20) (fps 50)
