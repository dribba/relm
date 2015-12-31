module Game where

import Random exposing (Generator, Seed)

type alias MapSettings = {}
type alias Block = Int
type alias Grid = List (Int, Int)
type alias Tile = ((Int, Int), Block)
type alias Map = 
  { tiles: List Tile
  , charStPos: (Int, Int)
  , nextMapSeed: Seed
  }
  
emptyBlock : Block
emptyBlock = 0

wallBlock : Block
wallBlock = 1

charPos : Block
charPos = 2

generateEmptyGrid : Int -> Int -> Grid
generateEmptyGrid w h =
  let 
    addY x = List.map (x |> (,)) [0..(h-1)]
  in
    List.concatMap addY [0..(w-1)]

placeTile : Block -> (Int, Int) -> Tile
placeTile tile p = (p, tile)

grid : (Int, Int) -> Block -> List Tile
grid (w, h) tile = 
  List.map (placeTile tile) (generateEmptyGrid w h)

characterRndPositionGen : Int -> Int -> Generator (Int, Int)
characterRndPositionGen maxX maxY =
  Random.pair (Random.int 0 (maxX - 1)) (Random.int 0 (maxY - 1))

addCharPosition : (Int, Int) -> Map -> Map
addCharPosition pos map =
  { map | charStPos = pos }

generateMap : MapSettings -> Seed -> (Int, Int) -> Map
generateMap settings seed (width, height) =
  let
    tiles = grid (width, height) emptyBlock -- For now just an empty map
    startingPosition = Random.generate (characterRndPositionGen width height) seed
  in
    { tiles = tiles
    , charStPos = fst startingPosition
    , nextMapSeed = snd startingPosition
    }