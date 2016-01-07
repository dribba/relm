module Relm where

import Color exposing (..)
import Text
import Time exposing (..)
import Keyboard
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Math exposing (toDegrees)
import Game exposing (..)
import Random exposing (Seed)
import Array exposing (Array)

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
  }

type alias Character =
  { position: Game.Position 
  , velocity: Velocity
  , direction: Direction
  , angle: Int
  }

type alias State = 
  { scene: Scene
  , character: Character
  , gameMap: Map
  , mapCenter: Point
  , viewPortCenter: (Int, Int)
  }


mapSize = 10
mapMiddle = round (mapSize / 2) - 1


tileHeight = 40
tileWidth = 40

gameWidth = 480
gameHeight = 400

toCoords : (Int, Int) -> Point
toCoords point = 
  Game.tileToCoords tileWidth tileHeight point

toRelativeCoords : Int -> Int -> (Int, Int) -> Point
toRelativeCoords width height point =
  let 
    absCoords = toCoords point
  in
    { x = absCoords.x - (toFloat width / 2)
    , y = absCoords.y - (toFloat height / 2)
    }

initialState : Map -> State
initialState gameMap = 
  { scene = Intro
  , character = 
    { position = Debug.log "CharPos" (toCoords (mapMiddle, mapMiddle)) -- (toCoords gameMap.charStPos)
    , velocity = { x = 0, y = 0 }
    , direction = S
    , angle = 180
    }
  , gameMap = gameMap
  , mapCenter = toCoords (mapMiddle, mapMiddle) -- 40 is the middle, but tiles are 0-indexed
  , viewPortCenter = gameMap.charStPos
  }


-- VIEW
groundSprites : String
groundSprites = "img/ground.png"

grass : Element
grass = 
  croppedImage (240, 0) tileWidth tileHeight groundSprites


gameStats : State -> Element
gameStats state =
  "x: " ++ (toString state.character.position.x) ++ 
  " y: " ++ (toString state.character.position.y)  ++ 
  " dir: " ++ (toString state.character.direction)
  |> Text.fromString
  |> leftAligned

gameContainer : Element -> Element
gameContainer =
  container gameWidth gameHeight bottomLeft 

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
        |> outlined (solid black)--(solid (rgba 50 50 50 0.6))
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

blockToTile : Block -> Form
blockToTile block = 
  case block of
    Empty -> (rect 40 40) |> filled (rgb 100 100 100)
    Wall -> toForm empty -- Shouldn't happen for now


-- printMap map = 
relativeTo : Float -> Boundaries -> Boundaries
relativeTo coord (low, high) = 
  (coord + low, coord + high)

inBounds : Boundaries -> Float -> Bool
inBounds (low, high) coord = 
  coord >= low && coord <= high
  
isRendered : (Int, Int) -> Float -> Point -> Point -> Bool 
isRendered (viewPWidth, viewPHeight) drawARatio viewPort {x, y} =
  let 
    xBoundaries = ((toFloat viewPWidth) * drawARatio) |> round |> boundaries |> relativeTo viewPort.x
    yBoundaries = ((toFloat viewPHeight) * drawARatio) |> round |> boundaries |> relativeTo viewPort.x
  in
    (inBounds xBoundaries x) && (inBounds yBoundaries y)

placeTile : ((Int, Int) -> Point) -> Tile -> Form
placeTile adjustCoords tile = 
  (tile |> snd |> blockToTile) |> move (tile |> fst |> adjustCoords |> toTuple)

grid : (Int, Int) -> Element -> List Form
grid (w, h) tile = 
  List.concatMap (\ix -> List.map (\iy -> createAndPlace ix iy tile) [0..(h-1)])  [0..(w-1)] 

cross =
  [rect 15 2, rect 2 15] |> List.map (filled red) |> group
  
addMapCenter coords tiles =
  List.append tiles [cross]

stage : Map -> Form
stage map =
  let 
    mapWidth = mapSize * tileWidth
    mapHeight = mapSize * tileHeight
  in
    group (Array.toList (map.tiles |> Array.map (placeTile (toRelativeCoords mapWidth mapHeight))))
  
sprites state =
  group [ cross
        --, printStats (gameStats state)
        , mainCharacter state.character
        ]
  

  
  
view : State -> Element
view state =
  gameContainer <|
  collage gameWidth gameHeight
    [ --stage state.gameMap
     sprites state
    ]

boundaries : Int -> Boundaries
boundaries size =
  let 
    half = size |> toFloat |> (\s -> s / 2)
  in
    (-half, half)
    
-- UPDATE
updateCoordinate : Boundaries -> Float -> Float -> Float
updateCoordinate boundaries coord velocity =
  let 
    newCoord = coord + velocity
  in 
    clamp (fst boundaries) (snd boundaries) newCoord 

updatePosition : Input -> Character -> Character
updatePosition input character =
  let 
    position = character.position
    newPosition = { position |
                    x = updateCoordinate (boundaries gameWidth) position.x input.arrows.x,
                    y = updateCoordinate (boundaries gameHeight) position.y input.arrows.y 
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
  let
    inputFps = Signal.sampleOn updateFps inputSignal
    updateState = Signal.foldp update (initialState (emptyMap mapSize mapSize)) inputFps
    renderedView = Signal.sampleOn gameFps (Signal.map view updateState)
  in
    renderedView

-- KEYS
gameRawFps =
  40

gameFps =
  fps gameRawFps

updateFps : Signal Time
updateFps =
  fps (gameRawFps * 2)

inputSignal : Signal Input
inputSignal =
  let
    asFloats arrows = 
      { arrows = 
        { x = toFloat arrows.x, 
          y = toFloat arrows.y 
        }
      }
  in
    Signal.map asFloats Keyboard.arrows