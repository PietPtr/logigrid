module State where

import Prelude

import Data.List
import Data.Vec
import Data.Typelevel.Num.Reps (D2, D4, D6, D8)
import Data.Maybe (Maybe(..))
import Data.Show
import Data.Tuple
import Data.Boolean
import Data.Generic.Rep
import Data.Map as Map
import Graphics.Canvas (CanvasImageSource)
import Partial.Unsafe (unsafePartial)

type Coord = {
    x :: Int, y :: Int
}

type GameState = {
        tiles :: List Tile,
        ioSwitches :: List IOSwitch,
        routingSwitches :: List RoutingSwitch,
        verticalTracks :: List VerticalTrack,
        inputs :: List Input,
        outputs :: List Output,
        player :: Player,
        keyMap :: KeyMap,
        imageMap :: Map.Map String CanvasImageSource,
        dimensions :: { height :: Number, width :: Number }
    }

type Input = {
        x :: Int,
        y :: Int,
        statea :: Boolean, -- set by a player action
        stateb :: Boolean
    }

type Output = {
        x :: Int,
        y :: Int,
        state :: Boolean -- driven by some LUT output
    }

type Tile = {
        x :: Int,
        y :: Int,
        arity :: Int,
        config :: TileConfiguration,
        netState :: TileNetState
    }


type TileConfiguration = {
        lutConfig :: List Boolean,
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
        x :: Int,
        y :: Int
    }

type IOSwitch = {
        x :: Int,
        y :: Int,
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
        x :: Int,
        y :: Int,
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


tiles :: Int -> Int -> List Tile
tiles nx ny = fromFoldable $ map makeTile coords
    where
        coords = do
            x <- 0..(nx-1)
            y <- 0..(ny-1)
            pure $ Tuple (x * 2) (y * 2)
        

        makeTile (Tuple x y) = {
            x: x,
            y: y,
            arity: 2,
            config: {
                lutConfig: false : true : true : true : Nil,
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

ioSwitches :: Int -> Int -> List IOSwitch
ioSwitches nx ny = fromFoldable $ map makeSwitch coords
    where
        coords = do
            x <- 0..(nx-1)
            y <- 1..(ny-1)
            pure $ Tuple (x * 2) (y * 2 - 1)

        makeSwitch (Tuple x y) = {
            x: x,
            y: y,
            config: false +> false +> false +> false +> false +> false +> empty,
            wireState: {
                aWire: false,
                bWire: false,
                yWire: false,
                trackUp: false,
                trackDown: false
                
            }
        }

verticalTracks :: Int -> Int -> List VerticalTrack
verticalTracks nx ny = fromFoldable $ map makeTrack coords
    where
        coords = do
            x <- 1..(nx-1)
            y <- 0..(ny-1)
            pure $ Tuple (x * 2 - 1) (y * 2)

        makeTrack (Tuple x y) = {
            x: x, y: y
        }

routingSwitches :: Int -> Int -> List RoutingSwitch
routingSwitches nx ny = fromFoldable $ map makeSwitch coords
    where
        coords = do
            x <- 1..(nx-1)
            y <- 1..(ny-1)
            pure $ Tuple (x * 2 - 1) (y * 2 - 1)

        makeSwitch (Tuple x y) = {
            x: x,
            y: y,
            config: 
                false +> false +> false +> false +> 
                false +> false +> false +> false +> empty
        }

inputs :: Int -> Int -> List Input
inputs nx ny = fromFoldable $ map makeInput coords
    where
        coords = (_ * 2) <$> (0..(nx - 1))
        
        makeInput x = {
            x: x,
            y: (-1),
            statea: false,
            stateb: true
        }

outputs :: Int -> Int -> List Output
outputs nx ny = fromFoldable $ map makeOutput coords
    where
        coords = (_ * 2) <$> (0..(nx - 1))

        makeOutput x = {
            x: x,
            y: ny * 2 - 1,
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
    , f: (\_ s -> s) } :
    Nil)


-- TODO: doe dit nou maar met maps sukkol het is echt grondig stuk op deze manier
switchPortA :: {x :: Int, y :: Int } -> GameState -> GameState
-- switchPortA coords state = state { inputs { statea = not state.inputs.statea } }
switchPortA coords state = state { inputs = inputs' }
    where
        inputs' = updated : (filter equal state.inputs)

        toUpdate = unsafePartial $ case filter (\s -> not $ equal s) state.inputs of
            (elem : _) -> elem

        updated = toUpdate { statea = not toUpdate.statea }

        equal :: forall r . {x :: Int, y :: Int | r} -> Boolean
        equal inp = inp.x == coords.x && inp.y == coords.y