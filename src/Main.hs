module Main where

import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

debug = flip trace

-- | Data describing the state of the pong game.
data PongGame = Game
    { isPaused :: Bool          -- ^ whether the game is paused
    , ballLoc :: (Float, Float) -- ^ Pong ball (x,y) location
    , ballVel :: (Float, Float) -- ^ Pong ball (x,y) velocity
    , player1 :: Float          -- ^ Left player paddle height. Zero is middle of the screen
    , player2 :: Float          -- ^ Right player paddle height
    , paddleRect :: (Float, Float) -- ^ Paddle size (width, height)
    , playerIsMoving :: (Float, Float)
    } deriving Show

type Radius = Float
type Position = (Float, Float)
type RectDimensions = (Float, Float)
type PaddleHeights = (Float, Float) -- | First is paddle 1 height, second is paddle 2 height

width, height, offset :: Int
width = 300
height = 300
offset = 100

initialState :: PongGame
initialState = Game
    { isPaused = False
    , ballLoc = (-10, 30)
    , ballVel = (-70, -170)
    , player1 = 40
    , player2 = -80
    , paddleRect = (26, 86)
    , playerIsMoving = (0, 0)
    }

fps :: Int
fps = 120

window :: Display
window = InWindow "Pong game" (width, height) (offset, offset)

background :: Color
background = black

render :: PongGame -> Picture
render game = 
    pictures 
            [ ball
            , walls
            , makePaddle rose 120 $ player1 game
            , makePaddle orange (-120) $ player2 game
            ]
    where 
        ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
        ballColor = dark red

        wall :: Float -> Picture
        wall offset = 
            translate 0 offset $ color wallColor $ rectangleSolid 270 10

        wallColor = greyN 0.5
        walls = pictures [wall 150, wall (-150)]

        (rectWidth, rectHeight) = paddleRect game

        makePaddle :: Color -> Float -> Float -> Picture
        makePaddle col x y = pictures
            [ translate x y $ color col $ rectangleSolid rectWidth rectHeight
            , translate x y $ color paddleColor $ rectangleSolid (rectWidth - 6) (rectHeight - 6)
            ]

        paddleColor = light $ light blue

moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y') }
    where 
        -- old locations and velocties
        (x, y) = ballLoc game
        (vx, vy) = ballVel game
        
        -- New locations
        x' = x + vx * seconds
        y' = y + vy * seconds

movePaddles :: Float -> PongGame -> PongGame
movePaddles seconds game = game { player1 = p1', player2 = p2' }
    where
        (vp1, vp2) = playerIsMoving game -- "velocities" of player paddles
        (paddleWidth, paddleHeight) = paddleRect game
        p1 = player1 game -- height of player 1 paddle
        p2 = player2 game -- ------||-------- 2 --||--

        screenHalfHeight = (fromIntegral height / 2) - 6
        paddleHalfHeight = paddleHeight / 2
        p1upper = p1 + paddleHalfHeight
        p1lower = p1 - paddleHalfHeight

        p2upper = p2 + paddleHalfHeight
        p2lower = p2 - paddleHalfHeight

        p1MayMoveUp = p1upper < screenHalfHeight && vp1 > 0
        p1MayMoveDown = p1lower > -screenHalfHeight && vp1 < 0

        p2MayMoveUp = p2upper < screenHalfHeight && vp2 > 0
        p2MayMoveDown = p2lower > -screenHalfHeight && vp2 < 0

        p1' = if p1MayMoveUp || p1MayMoveDown then p1 + vp1 * seconds else p1
        p2' = if p2MayMoveUp || p2MayMoveDown then p2 + vp2 * seconds else p2

wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
    where
        topCollision = y - radius <= -(fromIntegral height / 2)
        bottomCollision = y + radius >= (fromIntegral height / 2)

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
    where
        radius = 10
        (vx, vy) = ballVel game
        
        vy' = if wallCollision (ballLoc game) radius
            then
                -- update the velocity
                -vy
            else
                -- Return the old velocity
                vy

paddleCollision :: Position -> Radius -> PaddleHeights -> RectDimensions -> Bool
paddleCollision (ballX, ballY) radius (paddle1Offset, paddle2Offset) (paddleWidth, paddleHeight)  = leftCollision || rightCollision
    where
        diameter = (radius * 2)
        paddleFace = 120 - paddleWidth

        xLeftCollision = (ballX - radius) <= -paddleFace - diameter
        xRightCollision = (ballX - radius) >= paddleFace

        paddleHalfHeight = paddleHeight / 2
        
        -- it's just implemented as a collision line now 
        yLeftCollision = paddle2Offset + paddleHalfHeight >= ballY && paddle2Offset - paddleHalfHeight <= ballY
        yRightCollision = paddle1Offset + paddleHalfHeight >= ballY && paddle1Offset - paddleHalfHeight <= ballY

        leftCollision = xLeftCollision && yLeftCollision
        rightCollision = xRightCollision && yRightCollision

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = ( vx', vy ) }
    where
        radius = 10
        (vx, vy) = ballVel game
        
        vx' = if paddleCollision (ballLoc game) radius (player1 game, player2 game) (paddleRect game)
            then
                -vx
            else
                vx

update :: Float -> PongGame -> PongGame
update seconds game 
    | paddlesMoving = paddleBounce . wallBounce . (movePaddles seconds) . (moveBall seconds) $ game
    | isPaused game = game
    | otherwise = paddleBounce . wallBounce . (moveBall seconds) $ game
    where
        paddlesMoving = (fst $ playerIsMoving game) /= 0 || (snd $ playerIsMoving game) /= 0 

handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'q') _ _ _) game = 
    game { ballLoc = (0, 0) }
handleKeys (EventKey (Char 'p') (Up) _ _) game = 
    game { isPaused = (not $ isPaused game) }
handleKeys (EventKey (Char 'w') (Up) _ _) game = 
    game { playerIsMoving = ((fst $ playerIsMoving game), 0) }
handleKeys (EventKey (Char 'w') (Down) _ _) game =
    game { playerIsMoving = ((fst $ playerIsMoving game), 100) }
handleKeys (EventKey (Char 's') (Up) _ _) game = 
    game { playerIsMoving = ((fst $ playerIsMoving game), 0) }
handleKeys (EventKey (Char 's') (Down) _ _) game =
    game { playerIsMoving = ((fst $ playerIsMoving game), (-100)) }
handleKeys (EventKey (SpecialKey KeyDown) (Up) _ _) game = 
    game { playerIsMoving = (0, (snd $ playerIsMoving game)) }
handleKeys (EventKey (SpecialKey KeyDown) (Down) _ _) game = 
    game { playerIsMoving = ((-100), (snd $ playerIsMoving game)) }
handleKeys (EventKey (SpecialKey KeyUp) (Up) _ _) game = 
    game { playerIsMoving = (0, (snd $ playerIsMoving game)) }
handleKeys (EventKey (SpecialKey KeyUp) (Down) _ _) game = 
    game { playerIsMoving = (100, (snd $ playerIsMoving game)) }
handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update

