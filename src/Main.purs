module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Maybe (Maybe(..))
import Graphics.Canvas (rect, fillPath, setFillStyle, getContext2D,
    getCanvasElementById, setStrokeStyle, Context2D,
    clearRect, CanvasElement, getCanvasDimensions,
    setCanvasDimensions, strokePath, tryLoadImage, drawImage,
    drawImageScale, setFont, fillText, strokeText)
import Partial.Unsafe (unsafePartial)
import Effect.Random
import Data.Int hiding (pow)
import Data.List
import Data.Map as Map

import Web.HTML (window)
import Web.HTML.Window (Window, document, requestAnimationFrame,
    innerWidth, innerHeight)
import Web.HTML.HTMLElement (toElement, HTMLElement)
import Web.Event.Event
import Web.Event.EventTarget
import Web.HTML.Window (toEventTarget)
import Web.DOM.Element hiding (id)
import State
import Data.Foldable hiding (length)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Unsafe.Coerce
import Web.UIEvent.KeyboardEvent
import Signal.Time
import Data.Tuple
import Data.Typelevel.Num.Reps
import Data.Vec as Vec
import Math (pow)
import Debug

images = manual <> tileActives
    where
        manual = 
            ( "resources/tile.svg" 
            : "resources/ioswitch.svg" 
            : "resources/router.svg" 
            : "resources/track.svg"
            : "resources/crossbar.svg" 
            : "resources/one.svg"
            : "resources/zero.svg"
            : "resources/input.svg"
            : "resources/input_a.svg"
            : "resources/input_b.svg"
            : "resources/output.svg" 
            : "resources/muxreg.svg"
            : "resources/muxlut.svg"
            : Nil)
        
        tileActives = (\x -> "resources/tile/" <> x <> ".svg") <$> 
            ("porta" : "portb" : "lutOut" : "regOut" : "muxOut" : Nil)

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

    state <- Ref.new (defaultState { dimensions { height = toNumber windowHeight, width = toNumber windowWidth}})
    

    setKeyEvents state (toEventTarget w)
    setFont ctx "15px monospace"

    loadImages state images

    _ <- requestAnimationFrame (execFrame 0.0 state canvas w) w

    pure unit

loadImages :: Ref GameState -> List String -> Effect Unit
loadImages stateRef fileNames = do
    traverse_ loadImage fileNames
    where
        loadImage name = do
            tryLoadImage name callback
            where
                callback Nothing = pure unit
                callback (Just source) = do
                    _ <- Ref.modify (\state -> 
                        state {imageMap = Map.insert name source state.imageMap}) 
                        stateRef
                    pure unit


execFrame :: Number -> Ref GameState -> CanvasElement -> Window -> Effect Unit
execFrame previousMillis stateRef canvas w = do
    millis <- now

    state <- Ref.read stateRef
    dims <- getCanvasDimensions canvas
    -- log (show $ Tuple (state.player.x + dims.width / 2.0) (state.player.y + dims.height / 2.0))

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
        tiles = updateTiles state,
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

        speed = 337.5

updateTiles :: GameState -> List Tile
updateTiles state = map (updateTile state) state.tiles

b2i true = 1
b2i false = 0

updateTile :: GameState -> Tile -> Tile
updateTile state tile = tile {
        netState {
            porta = porta',
            portb = portb',
            lutOut = lutOut',
            regOut = regOut',
            muxOut = muxOut'
        }
    }
    where
        porta' = findDriverValues.statea
        portb' = findDriverValues.stateb
        lutOut' = unsafePartial $ case tile.config.lutConfig !! lutID of
            Just value -> value
        -- TODO: if rising edge, copy value at input
        regOut' = tile.netState.regOut
        muxOut' = if tile.config.regmux
            then regOut'
            else lutOut'

        lutID = (b2i porta') * 2 + b2i portb'

        findDriverValues :: { statea :: Boolean, stateb :: Boolean }
        findDriverValues = if tile.y == 0
            then searchInputs
            else searchIOSwitches
        
        searchInputs = unsafePartial $ case filter (\inp -> inp.x == tile.x) state.inputs of
            (input : Nil) -> { statea: input.statea, stateb: input.stateb }

        searchIOSwitches = unsafePartial $ case filter (\sw -> sw.x == tile.x && sw.y == tile.y - 1) state.ioSwitches of
            (switch : Nil) -> { statea: switch.wireState.aWire, stateb: switch.wireState.bWire }
            (Nil) -> {statea: false, stateb: false}

tilesize = 300.0

drawFromImageMap :: GameState -> Context2D -> String -> Number -> Number -> Number -> Effect Unit
drawFromImageMap state ctx filename drawx drawy scale = 
    case Map.lookup ("resources/"<> filename <>".svg") (state.imageMap) of
        Just source -> drawImageScale ctx source drawx drawy scale scale
        Nothing -> log ("Cannot find image " <> filename <> ".svg in resources folder.")

drawTiles :: CanvasElement -> Ref GameState -> Effect Unit
drawTiles canvas stateRef = do
    state <- Ref.read stateRef
    ctx <- getContext2D canvas

    traverse_ (drawTile state ctx) state.tiles
    traverse_ (drawTileActives state ctx) state.tiles

    traverse_ (drawIOSwitch state ctx) state.ioSwitches
    traverse_ (drawRouter state ctx) state.routingSwitches
    traverse_ (drawTrack state ctx) state.verticalTracks
    traverse_ (drawInput state ctx) state.inputs
    traverse_ (drawOutput state ctx) state.outputs

    where
        drawTile state ctx tile = do
            drawSVGat "tile" tile state ctx
            
            -- drawFromImageMap state ctx "zero" drawx drawy tilesize
            traverse_ drawLutConfig (zip tile.config.lutConfig (0..3)) -- TODO: limits to 2LUTs.

            if tile.config.regmux
                then drawSVGat "muxreg" tile state ctx
                else drawSVGat "muxlut" tile state ctx

            -- tile.config.lutConfig
            where
                value b = if b then "one" else "zero"
                drawLutConfig (Tuple b n) = do
                    let (Tuple drawx drawy) = tilePos state tile.x tile.y
                    drawFromImageMap state ctx (value b) drawx (drawy + toNumber n * 0.126 * tilesize) tilesize

        drawIOSwitch state ctx switch = drawSVGat "ioswitch" switch state ctx
        drawRouter state ctx router = drawSVGat "router" router state ctx
        drawTrack state ctx track = drawSVGat "track" track state ctx
        drawInput state ctx input = do
            drawSVGat "input" input state ctx
            if input.statea
                then drawSVGat "input_a" input state ctx
                else pure unit
            if input.stateb
                then drawSVGat "input_b" input state ctx
                else pure unit

            -- if input.stateb
            --     then drawSVGat "input_b" input state ctx
        drawOutput state ctx output = drawSVGat "output" output state ctx

        drawSVGat :: forall r . String -> { x :: Int, y :: Int | r } -> GameState -> Context2D -> Effect Unit
        drawSVGat filename geval state ctx = do
            let (Tuple drawx drawy) = tilePos state geval.x geval.y

            setFillStyle ctx "#ffffff00"
            setStrokeStyle ctx "#cccccc"
            strokePath ctx $ rect ctx {
                x: drawx,
                y: drawy,
                width: tilesize,
                height: tilesize
            }

            drawFromImageMap state ctx filename drawx drawy tilesize


        drawTileActives :: GameState -> Context2D -> Tile -> Effect Unit
        drawTileActives state ctx tile = do
            ifDraw tile.netState.porta "porta"
            ifDraw tile.netState.portb "portb"
            ifDraw tile.netState.lutOut "lutOut"
            ifDraw tile.netState.regOut "regOut"
            ifDraw tile.netState.muxOut "muxOut"
                where
                    ifDraw isActive name = do
                        let (Tuple drawx drawy) = tilePos state tile.x tile.y
                        if isActive then
                            case Map.lookup ("resources/tile/"<> name <> ".svg") state.imageMap of
                                Just source -> drawImageScale ctx source drawx drawy tilesize tilesize
                                Nothing -> pure unit
                            else pure unit



        tilePos :: GameState -> Int -> Int -> Tuple Number Number
        tilePos state x y = Tuple drawx drawy
            where
                drawx = toNumber x * tilesize - state.player.x
                drawy = toNumber y * tilesize - state.player.y


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

keyPressHandler :: Ref GameState -> Event -> Effect Unit
keyPressHandler stateRef event = unsafePartial do
    let (Just keyEvent) = fromEvent event

    case code keyEvent of
        "KeyE" -> applyInteractable stateRef
        _ -> pure unit

setKeyEvents :: Ref GameState -> EventTarget -> Effect Unit
setKeyEvents stateRef target = do
    downListener <- eventListener (keyDownHandler stateRef)
    addEventListener (EventType "keydown") downListener false target

    upListener <- eventListener (keyUpHandler stateRef)
    addEventListener (EventType "keyup") upListener false target

    pressListener <- eventListener (keyPressHandler stateRef)
    addEventListener (EventType "keypress") pressListener false target


applyInteractable :: Ref GameState -> Effect Unit
applyInteractable stateRef = do
    state <- Ref.read stateRef
    _ <- Ref.modify (findAndApply) stateRef
    pure unit
    where
        findAndApply :: GameState -> GameState
        findAndApply state = stateFunc state
            where
                playerx' = state.player.x + state.dimensions.width / 2.0
                playery' = state.player.y + state.dimensions.height / 2.0

                -- find the int coordinates of the tile we're in
                coords :: { x :: Int, y :: Int }
                coords = {
                    x: floor $ playerx' / tilesize,
                    y: floor $ playery' / tilesize
                }

                -- find what type it is
                tileType = unsafePartial $ case filter equalCoords state.inputs of
                    (inp : _ ) -> GridInput
                    Nil -> case filter equalCoords state.tiles of
                        (tile : _ ) -> GridTile
                        Nil -> case filter equalCoords state.outputs of
                            (out : _ ) -> GridOutput
                            Nil -> case filter equalCoords state.ioSwitches of
                                (ioSwitch : _) -> GridIOSwitch
                                Nil -> case filter equalCoords state.routingSwitches of
                                    (router : _) -> GridRouter

                equalCoords :: forall r . { x :: Int, y :: Int | r } -> Boolean
                equalCoords e = e.x == coords.x && e.y == coords.y

                -- filter interactables for that type
                interactables = filter (\ia -> ia.gridType == tileType) interActions

                -- find closest interactable within reach
                stateFunc :: GameState -> GameState
                stateFunc = case minimumBy (\(Tuple a _) (Tuple b _) -> compare a b) 
                        (zip (map distanceFromPlayer interactables) interactables) of
                    -- apply its function to the gamestate to the tile we're in.
                    Just (Tuple dist ia) -> if dist < 0.2
                        then (ia.f coords)
                        else id
                    Nothing -> id

                relativePlayerPos = {
                    x: decPart $ playerx' / tilesize,
                    y: decPart $ playery' / tilesize
                }
                distanceFromPlayer ia = (ia.x - relativePlayerPos.x) `pow` 2.0 + (ia.y - relativePlayerPos.y) `pow` 2.0

        

decPart :: Number -> Number
decPart x = x - (toNumber $ floor x)

id :: forall a . a -> a
id a = a