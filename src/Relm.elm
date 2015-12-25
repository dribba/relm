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


tileHeight = 32
tileWidth = 32

gameHeight = 400
gameWidth = 480

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
gameStats state =
    "vx: " ++ (toString state.character.vx) ++ " vy: " ++ (toString state.character.vy) 
    |> Text.fromString
    |> leftAligned

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
    
view : State -> Element
view state =
    container gameWidth gameHeight middle <|
    collage gameWidth gameHeight
        [ printStats (gameStats state)
        , mainCharacter state.character
        ]


-- UPDATE
updatePosition : {x: Int, y: Int} -> Position -> Position
updatePosition {x, y} position =
    { position |
        x = position.x + x,
        y = position.y + y  
    }

updateCharacter : {x: Int, y: Int} -> Character -> Character
updateCharacter ({x, y} as speed) character = 
    { character |
        vx = x,
        vy = y,
        position = updatePosition speed character.position
    }

update : {x: Int, y: Int} -> State -> State
update speed state =
    { state | character = updateCharacter speed state.character}


-- MAIN
main =
    Signal.map view (Signal.foldp update initialState clock)


-- KEYS
input : Signal { x: Int, y: Int }
input = 
    Keyboard.arrows

clock : Signal {x: Int, y: Int}    
clock =
    Signal.map2 (\dt arrows -> arrows) (fps 30) input
