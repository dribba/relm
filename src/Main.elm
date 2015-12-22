module Main where

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

view state = 
    show ("Awesome stats = vx: " ++ (toString state.character.vx) ++ " vy: " ++ (toString state.character.vy)) 

update : {x: Int, y: Int} -> State -> State
update ({x, y} as pos) state =
    let 
        oldCharacter = state.character
        newCharacter = { oldCharacter |
                            vx = pos.x, 
                            vy = pos.y
                       }
    in
        { state | character = newCharacter }

main =
    Signal.map view (Signal.foldp update initialState clock)
    

input = 
    Keyboard.arrows
    
clock =
    Signal.map2 (\dt arrows -> arrows) (fps 30) input
    
    