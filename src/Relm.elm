module Relm where

import Color exposing (..)
import Text
import Time exposing (..)
import Keyboard
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Math exposing (toDegrees)
import Game exposing (Map)
import Random exposing (Seed)

type Scene 
  = Intro 
  | Stage Int
  
type alias Point = 
  { x: Float
  , y: Float
  }

type alias Velocity = Point
type alias Position = Point

type alias Boundaries = (Float, Float)

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

type alias Character =
  { position: Position 
  , velocity: Velocity
  , direction: Direction
  , angle: Int
  }

type alias State = 
  { scene: Scene
  , character: Character
  , gameMap: Map
  }


tileHeight = 40
tileWidth = 40

gameWidth = 480
gameHeight = 400

initialState gameMap = 
  { scene = Intro
  , character = 
    { position =  { x = 50, y = 50 }
    , velocity = { x = 0, y = 0 }
    , direction = S
    , angle = 180
    }
  , gameMap = gameMap
  }


-- VIEW
groundSprites : String
groundSprites = "img/ground.png"

grass : Element
grass = 
  croppedImage (240, 0) tileWidth tileHeight groundSprites


gameStats : State -> Element
gameStats state =
  "vx: " ++ (toString state.character.velocity.x) ++ 
  " vy: " ++ (toString state.character.velocity.y)  ++ 
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
  group [ circle 10
        |> outlined (solid (rgba 50 50 50 0.6))
        |> move (character.position.x, character.position.y)
        
      , direction character 
        |> move (character.position.x, character.position.y)
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
    (-half, half)
    
-- UPDATE
updateCoordinate : Boundaries -> Time -> Float -> Float -> Float
updateCoordinate boundaries time coord velocity =
  let 
    newCoord = coord + velocity * time
  in 
    clamp (fst boundaries) (snd boundaries) newCoord 

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
              (atan2 input.arrows.x input.arrows.y) |> toDegrees |> round
    direction = angleToDirection angle 
  in
    { character | 
      position = newPosition,
      direction = direction,
      angle = angle
    }

updateVelocity : Input -> Character -> Character
updateVelocity input character =
  let 
    velocity = character.velocity
    newVel = { velocity | 
               x = input.arrows.x,
               y = input.arrows.y
             }
  in
    { character | velocity = newVel }


update : Input -> State -> State
update input state =
  { state | 
    character = state.character 
            |> updateVelocity input
            |> updatePosition input
  }

-- MAIN
emptyMap : Int -> Int -> Map
emptyMap width height = 
  Debug.log "Map: " (Game.generateMap {} (Random.initialSeed 12345) (width, height))

main : Signal Element
main =
  Signal.map view (Signal.foldp update (initialState (emptyMap 80 80)) input)


-- KEYS
input : Signal Input
input = 
  Signal.sampleOn delta (Signal.map2 (\time arrows -> { time = time
                                                      , arrows = { x = toFloat arrows.x
                                                                 , y = toFloat arrows.y
                                                                 } 
                                                      }) delta Keyboard.arrows )

delta : Signal Time  
delta =
  Signal.map (\t -> t / 20) (fps 50)
