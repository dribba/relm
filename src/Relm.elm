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
    
view : State -> Element
view state =
    container gameWidth gameHeight middle <|
    collage gameWidth gameHeight
        [ toForm(gameStats state) |> move (50 - gameWidth / 2, 20 - gameHeight / 2)
        , mainCharacter state.character
        ]

update : {x: Int, y: Int} -> State -> State
update {x, y} state =
    let 
        oldPosition = state.character.position
        newPosition = { oldPosition |
                        x = oldPosition.x + x,
                        y = oldPosition.y + y 
                      }
        oldCharacter = state.character
        newCharacter = { oldCharacter |
                            vx = x,
                            vy = y,
                            position = newPosition
                       }
    in
        { state | character = newCharacter }

main =
    Signal.map view (Signal.foldp update initialState clock)
    

input = 
    Keyboard.arrows

speed = 3

clock : Signal {x: Int, y: Int}    
clock =
    Signal.map2 (\dt arrows -> { arrows | x = arrows.x * speed, y = arrows.y * speed }) (fps 30) input
    
    