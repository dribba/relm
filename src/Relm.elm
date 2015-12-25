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
    = North 
    | South
    | East
    | West 

type alias Position = 
    { x: Int
    , y: Int
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
        , direction = South
        }
    }


-- VIEW
groundSprites : String
groundSprites = "img/ground.png"

grass : Element
grass = croppedImage (240, 0) tileWidth tileHeight groundSprites


gameStats : State -> Element
gameStats state =
    "vx: " ++ (toString state.character.vx) ++ " vy: " ++ (toString state.character.vy) 
    |> Text.fromString
    |> leftAligned

gameContainer : Element -> Element
gameContainer =
    container gameWidth gameHeight middle
    
mainCharacter : Character -> Form
mainCharacter character =
    rect 20 20 
    |> filled red
    |> move (toFloat character.position.x, toFloat character.position.y)

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


-- UPDATE
updatePosition : Velocity -> Position -> Position
updatePosition {x, y} position =
    { position |
        x = position.x + x,
        y = position.y + y  
    }

updateCharacter : Velocity -> Character -> Character
updateCharacter ({x, y} as speed) character = 
    { character |
        vx = x,
        vy = y,
        position = updatePosition speed character.position
    }

update : Velocity -> State -> State
update speed state =
    { state | character = updateCharacter speed state.character}


-- MAIN
main : Signal Element
main =
    Signal.map view (Signal.foldp update initialState clock)


-- KEYS
input : Signal Velocity
input = 
    Keyboard.arrows

clock : Signal Velocity    
clock =
    Signal.map2 (\dt arrows -> arrows) (fps 30) input
