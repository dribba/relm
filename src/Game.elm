module Game where

import Random exposing (Generator, Seed)
import Array exposing (..)

type alias MapSettings = {}

type Block = 
  Empty
  | Wall
  

type alias Point = 
  { x: Float
  , y: Float
  }

type alias Velocity = Point
type alias Position = Point

type alias Boundaries = (Float, Float)

type alias Grid = Array (Int, Int)
type alias Tile = ((Int, Int), Block)
type alias Map = 
  { tiles: Array Tile
  , charStPos: (Int, Int)
  , nextMapSeed: Seed
  }

arrayConcatMap : (a -> Array b) -> Array a -> Array b
arrayConcatMap fn xs =
  let 
    mergeThem el ys = Array.append ys (fn el)
  in
    Array.foldl mergeThem Array.empty xs

toTuple : Point -> (Float, Float)
toTuple {x, y} = (x, y)

generateEmptyGrid : Int -> Int -> Grid
generateEmptyGrid w h =
  let 
    addY x = initialize (h - 1) ((,) x)
  in
    arrayConcatMap addY (initialize (w - 1) identity)

placeTile : Block -> (Int, Int) -> Tile
placeTile tile p = (p, tile)

tileToCoords : Int -> Int -> (Int, Int) -> Point
tileToCoords tileW tileH (x, y) =
  let
    (width, height) = (toFloat tileW, toFloat tileH)
    (offsetX, offsetY) = (width / 2, height / 2)
  in
    { x = (toFloat x) * width + offsetX
    , y = (toFloat y) * height + offsetY
    }

grid : (Int, Int) -> Block -> Array Tile
grid (w, h) tile = 
  Array.map (placeTile tile) (generateEmptyGrid w h)

characterRndPositionGen : Int -> Int -> Generator (Int, Int)
characterRndPositionGen maxX maxY =
  Random.pair (Random.int 0 (maxX - 1)) (Random.int 0 (maxY - 1))

addCharPosition : (Int, Int) -> Map -> Map
addCharPosition pos map =
  { map | charStPos = pos }

generateMap : MapSettings -> Seed -> (Int, Int) -> Map
generateMap settings seed (width, height) =
  let
    tiles = grid (width, height) Empty -- For now just an empty map
    startingPosition = Random.generate (characterRndPositionGen width height) seed
  in
    { tiles = tiles
    , charStPos = fst startingPosition
    , nextMapSeed = snd startingPosition
    }