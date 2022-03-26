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

import Web.HTML (window)
import Web.HTML.Window (Window, document, requestAnimationFrame,
    innerWidth, innerHeight)
import Web.HTML.HTMLElement (toElement, HTMLElement)
import Web.Event.Event
import Web.Event.EventTarget
import Web.HTML.Window (toEventTarget)
import Web.DOM.Element
import State
import Data.Foldable
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Unsafe.Coerce
import Web.UIEvent.KeyboardEvent
import Signal.Time

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

    state <- Ref.new defaultState

    setKeyEvents state (toEventTarget w)

    _ <- requestAnimationFrame (execFrame 0.0 state canvas w) w

    pure unit


execFrame :: Number -> Ref GameState -> CanvasElement -> Window -> Effect Unit
execFrame previousMillis stateRef canvas w = do
    millis <- now

    update ((millis - previousMillis) / 1000.0) stateRef
    draw stateRef canvas
    _ <- requestAnimationFrame (execFrame millis stateRef canvas w) w
    pure unit


draw :: Ref GameState -> CanvasElement -> Effect Unit
draw stateRef canvas = do
    ctx <- getContext2D canvas
    dims <- getCanvasDimensions canvas
    clearRect ctx { x: 0.0, y: 0.0, width: dims.width, height: dims.height}

    drawTiles canvas stateRef
    drawPlayer canvas stateRef

    pure unit

update :: Number -> Ref GameState -> Effect Unit
update dt stateRef = do
    _ <- Ref.modify (updateGameState dt) stateRef
    pure unit

updateGameState :: Number -> GameState -> GameState
updateGameState dt state = state {
        -- tiles = updateTiles state.tiles,
        -- ioSwitches = updateIOSwitches state.ioSwitches,
        -- routingSwitches = updateRoutingSwitches state.routingSwitches,
        player = updatePlayer dt state
    }

updatePlayer :: Number -> GameState -> Player
updatePlayer dt state = state.player {
        x = state.player.x + horizontal * dt * speed,
        y = state.player.y + vertical * dt * speed
    }
    where
        horizontal = 
            if state.keyMap.left then (-1.0) else (
                if state.keyMap.right then 1.0 else 0.0)
        vertical = 
            if state.keyMap.up then (-1.0) else (
                if state.keyMap.down then 1.0 else 0.0)

        speed = 400.0

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

    setFillStyle ctx "#2e82e8"
    fillPath ctx $ rect ctx
        { x: dims.width / 2.0
        , y: dims.height / 2.0
        , width: 35.0
        , height: 35.0
        }


keyHandler :: Boolean -> Ref GameState -> Event -> Effect Unit
keyHandler b state event = unsafePartial do
    let (Just keyEvent) = fromEvent event
    _ <- Ref.modify (updateKeyMap $ code keyEvent) state
    pure unit
    where
        updateKeyMap keyCode state = case keyCode of
            "KeyW" -> state { keyMap {up = b} }
            "KeyS" -> state { keyMap {down = b} }
            "KeyA" -> state { keyMap {left = b} }
            "KeyD" -> state { keyMap {right = b} }
            _ -> state

keyDownHandler :: Ref GameState -> Event -> Effect Unit
keyDownHandler = keyHandler true

keyUpHandler :: Ref GameState -> Event -> Effect Unit
keyUpHandler = keyHandler false

setKeyEvents :: Ref GameState -> EventTarget -> Effect Unit
setKeyEvents state target = do
    downListener <- eventListener (keyDownHandler state)
    addEventListener (EventType "keydown") downListener false target

    upListener <- eventListener (keyUpHandler state)
    addEventListener (EventType "keyup") upListener false target