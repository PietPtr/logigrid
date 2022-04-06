module State where

import Prelude

import Data.List
import Data.Vec hiding (zip)
import Data.List.Lazy (repeat)
import Data.Typelevel.Num.Reps (D2, D4, D6, D8)
import Data.Maybe (Maybe(..))
import Data.Show
import Data.Tuple
import Data.Boolean
import Data.Generic.Rep
import Data.Ord
import Data.Map as Map
import Data.Map (Map)
import Graphics.Canvas (CanvasImageSource)
import Partial.Unsafe (unsafePartial)

type Coord = {
    x :: Int, y :: Int
}

type GameState = {
        tiles :: Map Coord Tile,
        ioSwitches :: Map Coord IOSwitch,
        routingSwitches :: Map Coord RoutingSwitch,
        verticalTracks :: Map Coord VerticalTrack,
        inputs :: Map Coord Input,
        outputs :: Map Coord Output,
        player :: Player,
        keyMap :: KeyMap,
        imageMap :: Map String CanvasImageSource,
        dimensions :: { height :: Number, width :: Number }
    }

type Input = {
        statea :: Boolean, -- set by a player action
        stateb :: Boolean
    }

type Output = {
        state :: Boolean -- driven by some LUT output
    }

type Tile = {
        arity :: Int,
        config :: TileConfiguration,
        netState :: TileNetState
    }


type TileConfiguration = {
        lutConfig :: Map Int Boolean,
        regmux :: Boolean
    }

type TileNetState = {
        porta :: Boolean,
        portb :: Boolean,
        lutOut :: Boolean,
        regOut :: Boolean,
        muxOut :: Boolean
    }

type VerticalTrack = {
        -- ooit iets van state nodig i guess?
    }

type IOSwitch = {
        config :: Vec D6 Boolean, -- TODO: support different switch box topologies
        -- TODO: wirestate object
        -- at least can easily be used to render the activity of wires, how exactly it is going to be
        -- populated remains to be seen. Probably starting at the driving output of the tile up north,
        -- and then continue somehow.
        wireState :: IOWireState
    }

type IOWireState = {
        aWire :: Boolean,
        bWire :: Boolean,
        yWire :: Boolean,
        trackUp :: Boolean,
        trackDown :: Boolean
    }

type RoutingSwitch = {
        config :: Vec D8 Boolean
        -- TODO: portstate object
    }

type Player = {
        x :: Number,
        y :: Number
    }

-- Set by keydown and keyup events, keeps track of states of keys that are continuously pressed
-- down by the player.
type KeyMap = {
        up :: Boolean,
        left :: Boolean,
        right :: Boolean,
        down :: Boolean
    }

defaultState :: GameState
defaultState = {
        tiles: tiles 3 3,
        ioSwitches: ioSwitches 3 3,
        routingSwitches: routingSwitches 3 3,
        verticalTracks: verticalTracks 3 3,
        inputs: inputs 3 3,
        outputs: outputs 3 3,
        player: {
            x: 0.0,
            y: 0.0
        },
        keyMap: {
            up: false,
            left: false,
            right: false,
            down: false
        },
        imageMap: Map.empty,
        dimensions: {
            height: 0.0,
            width: 0.0
        }
    }


tiles :: Int -> Int -> Map Coord Tile
tiles nx ny = Map.fromFoldable $ zip coords (map makeTile coords)
    where
        coords = do
            x <- 0..(nx-1)
            y <- 0..(ny-1)
            pure { x: x * 2, y: y * 2 }
        

        makeTile _ = {
            arity: 2,
            config: {
                lutConfig: Map.fromFoldable (Tuple 0 false : Tuple 1 false : Tuple 2 false : Tuple 3 false : Nil),
                regmux: false
            },
            netState: {
                porta: false,
                portb: false,
                lutOut: false,
                regOut: false,
                muxOut: false
            }
        }

ioSwitches :: Int -> Int -> Map Coord IOSwitch
ioSwitches nx ny = Map.fromFoldable $ zip coords (map makeSwitch coords)
    where
        coords = do
            x <- 0..(nx-1)
            y <- 1..(ny-1)
            pure { x: x * 2, y: y * 2 - 1 }

        makeSwitch _ = {
            config: false +> false +> false +> false +> false +> false +> empty,
            wireState: {
                aWire: false,
                bWire: false,
                yWire: false,
                trackUp: false,
                trackDown: false
                
            }
        }

verticalTracks :: Int -> Int -> Map Coord VerticalTrack
verticalTracks nx ny = Map.fromFoldable $ zip coords (map makeTrack coords)
    where
        coords = do
            x <- 1..(nx-1)
            y <- 0..(ny-1)
            pure $ { x: (x * 2 - 1), y: (y * 2) }

        makeTrack _ = {
        }

routingSwitches :: Int -> Int -> Map Coord RoutingSwitch
routingSwitches nx ny = Map.fromFoldable $ zip coords (map makeSwitch coords)
    where
        coords = do
            x <- 1..(nx-1)
            y <- 1..(ny-1)
            pure { x: x * 2 - 1, y: y * 2 - 1 }

        makeSwitch _ = {
            config: 
                false +> false +> false +> false +> 
                false +> false +> false +> false +> empty
        }

inputs :: Int -> Int -> Map Coord Input
inputs nx ny = Map.fromFoldable $ zip coords (map makeInput coords)
    where
        coords = do
            x <- 0..(nx-1)
            pure { x: x * 2, y: -1}
        
        makeInput _ = {
            statea: false,
            stateb: true
        }

outputs :: Int -> Int -> Map Coord Output
outputs nx ny = Map.fromFoldable $ zip coords (map makeOutput coords)
    where
        coords = do
            x <- 0..(nx - 1)
            pure { x: 2 * x, y: ny * 2 - 1 }

        makeOutput _ = {
            state: false
        }

data GridType = GridTile | GridInput | GridOutput | GridIOSwitch | GridRouter
derive instance Eq GridType


type InterAction = {
        gridType :: GridType,
        x :: Number, -- relative to topleft of parent tile
        y :: Number,
        f :: {x :: Int, y :: Int } -> GameState -> GameState
    }

interActions = (
    { gridType: GridInput -- port a driver
    , x: 0.165, y: 0.575
    , f: switchPortA } :
    { gridType: GridInput -- port b driver
    , x: 0.835, y: 0.575
    , f: switchPortB } :
    { gridType: GridTile
    , x: 0.632, y: 0.789
    , f: switchMux } :
    { gridType: GridTile
    , x: 0.310, y: 0.110
    , f: switchLUT00 } :
    { gridType: GridTile
    , x: 0.310, y: 0.260
    , f: switchLUT01 } :
    { gridType: GridTile
    , x: 0.310, y: 0.371
    , f: switchLUT10 } :
    { gridType: GridTile
    , x: 0.310, y: 0.502
    , f: switchLUT11 } :
    Nil)


switchPortA = switchInInput (\tile -> tile { statea = not tile.statea })
switchPortB = switchInInput (\tile -> tile { stateb = not tile.stateb })

switchInInput :: (Input -> Input) -> {x :: Int, y :: Int} -> GameState -> GameState
switchInInput f coords state = state { inputs = inputs' }
    where
        inputs' = Map.insert coords (f tile) state.inputs
        tile = findInMap coords state.inputs
 
switchMux = switchInTile (\tile -> tile { config { regmux = not tile.config.regmux }})
switchLUT00 = switchLUTXX 0
switchLUT01 = switchLUTXX 1
switchLUT10 = switchLUTXX 2
switchLUT11 = switchLUTXX 3

switchLUTXX x = switchInTile (\tile -> 
    tile { config { lutConfig = Map.update (\v -> Just $ not v) x tile.config.lutConfig }})


switchInTile :: (Tile -> Tile) -> {x :: Int, y :: Int } -> GameState -> GameState
switchInTile f coords state = state { tiles = tiles' }
    where
        tiles' = Map.insert coords (f tile) state.tiles
        tile = findInMap coords state.tiles

findInMap :: forall k v . Ord k => k -> Map k v -> v
findInMap coords map = unsafePartial $ case Map.lookup coords map of
    Just t -> t