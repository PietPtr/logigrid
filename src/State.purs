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


type GameState = {
        tiles :: List Tile,
        ioSwitches :: List IOSwitch,
        routingSwitches :: List RoutingSwitch,
        player :: Player,
        keyMap :: KeyMap
    }


type Tile = {
        x :: Int,
        y :: Int,
        arity :: Int,
        config :: TileConfiguration
    }

type TileConfiguration = {
        lutConfig :: Vec D2 (Maybe Boolean), -- ugly way to at most support 6LUTs.
        regmux :: Boolean
    }

type IOSwitch = {
        x :: Int,
        y :: Int,
        config :: Vec D6 Boolean -- TODO: support different switch box topologies
    }

type RoutingSwitch = {
        x :: Int,
        y :: Int,
        config :: Vec D8 Boolean
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
        player: {
            x: 0.0,
            y: 0.0
        },
        keyMap: {
            up: false,
            left: false,
            right: false,
            down: false
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
                lutConfig: Nothing +> Nothing +> empty,
                regmux: false
            }
        }

ioSwitches :: Int -> Int -> List IOSwitch
ioSwitches nx ny = fromFoldable $ map makeSwitch coords
    where
        coords = do
            x <- 1..(nx-1)
            y <- 0..(ny-1)
            pure $ Tuple (x * 2 - 1) (y * 2)

        makeSwitch (Tuple x y) = {
            x: x,
            y: y,
            config: false +> false +> false +> false +> false +> false +> empty
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
