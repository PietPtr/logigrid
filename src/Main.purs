module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Maybe (Maybe(..))
import Graphics.Canvas (rect, fillPath, setFillStyle, getContext2D,
    getCanvasElementById, setStrokeStyle, Context2D,
    clearRect, CanvasElement, getCanvasDimensions,
    setCanvasDimensions, strokePath)
import Partial.Unsafe (unsafePartial)
import Effect.Random
import Data.Int
import Data.List

import Web.HTML.Window (Window, document, requestAnimationFrame,
    innerWidth, innerHeight)
import Web.HTML (window)
import Web.UIEvent.MouseEvent.EventTypes
import State
import Data.Foldable
import Effect.Ref as Ref
import Effect.Ref (Ref)

main :: Effect Unit
main = void $ unsafePartial do
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    w <- window

    windowWidth <- innerWidth w
    windowHeight <- innerHeight w
    _ <- setCanvasDimensions canvas { height: toNumber windowHeight, width: toNumber windowWidth}

    setFillStyle ctx "#abc9ee"
    setStrokeStyle ctx "#000000"

    -- let target = elementToEventTarget canvas

    -- addEventListener 

    state <- Ref.new defaultState

    _ <- requestAnimationFrame (execFrame state canvas w) w

    pure unit


execFrame :: Ref GameState -> CanvasElement -> Window -> Effect Unit
execFrame stateRef canvas w = do
    draw stateRef canvas
    _ <- requestAnimationFrame (execFrame stateRef canvas w) w
    pure unit


draw :: Ref GameState -> CanvasElement -> Effect Unit
draw stateRef canvas = do
    ctx <- getContext2D canvas
    dims <- getCanvasDimensions canvas
    clearRect ctx { x: 0.0, y: 0.0, width: dims.width, height: dims.height}

    drawTiles canvas stateRef
    drawPlayer canvas stateRef

    pure unit

tilesize = 200.0

drawTiles :: CanvasElement -> Ref GameState -> Effect Unit
drawTiles canvas stateRef = do
    state <- Ref.read stateRef
    ctx <- getContext2D canvas
    
    setFillStyle ctx "#ffffff00"
    setStrokeStyle ctx "#000000"

    traverse_ (drawTile state ctx) state.tiles

    where
        drawTile :: GameState -> Context2D -> Tile -> Effect Unit
        drawTile state ctx tile = do
            strokePath ctx $ rect ctx {
                x: toNumber tile.x * tilesize - state.player.x,
                y: toNumber tile.y * tilesize - state.player.y,
                width: tilesize - 10.0,
                height: tilesize - 10.0
            }



drawPlayer :: CanvasElement -> Ref GameState -> Effect Unit
drawPlayer canvas stateRef = do
    state <- Ref.read stateRef
    ctx <- getContext2D canvas
    dims <- getCanvasDimensions canvas

    setFillStyle ctx "#abc9ee"
    setStrokeStyle ctx "#000000"
    fillPath ctx $ rect ctx
        { x: dims.width / 2.0
        , y: dims.height / 2.0
        , width: 50.0
        , height: 50.0
        }
        