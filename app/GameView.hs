{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module GameView (module GameView) where

import Geometry
import Textures
import Settings
import DataObjects
import GameCollisions
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Data.List
import Control.Lens
import Control.Concurrent
import Control.Monad

initGameState :: GameState -> GameState
initGameState game = game
  { _ball         = Ball { _ballPosition = (0, -300), _ballVelocity = (0, ballFullVelocity) }
  , _paddle       = Paddle { _paddlePosition = (0, -fromIntegral screenHeight / 2 + 50), _paddleVelocityX = 0} 
  , _score        = ("aaa", "0")
  , _bricks       = initBricks
  , _currentView  = GameInProgress
  , _nextRowColor = 4
  }

initBricks :: [Brick]
initBricks = [ Brick { _brickPosition = (brickPositionX j, brickPositionY i)
                     , _picture       = translate (brickPositionX j) (brickPositionY i) $ color (getColor i) $ rectangleSolid brickWidth brickHeight
                     , _row           = i
                     , _brickColor    = getColor i
                     } | j <- [0..brickCols-1], i <- [0..brickRows-1]]

update :: Float -> GameState -> IO GameState
update time = checkEndGame . brickBounce . wallBounce . paddleBounce . moveBall time

render :: GameState  -> Picture
render game = pictures $ [game ^. textures ^. backgroundGameTex, paddlePicture, ballPicture] ++ map (\brick -> brick ^. picture) (game ^. bricks) ++ (showScore game)
  where
    paddlePicture :: Picture
    paddlePicture = uncurry translate (game ^. paddle ^. paddlePosition) $ game ^. textures ^. paddleTex

    ballPicture :: Picture
    ballPicture = uncurry translate (game ^. ball  ^. ballPosition) $ game ^. textures ^. ballTex

class ShowScore a where
  showScore  :: a -> [Picture]

instance ShowScore GameState where
  showScore game = [ pictures $ [translate (scoreLeftOffset + fromIntegral (i * 38)) (fromIntegral screenHeight / 2 - 40) (snd $ (filter (\num -> (fst num) == scoreString!!i) (game ^. textures ^. numsTex))!!0) | i <- [0..length scoreString - 1]] ]
    where
      scoreString :: String
      scoreString = snd $ game ^. score

      scoreLeftOffset :: Float
      scoreLeftOffset = (fromIntegral screenWidth / 2 - fromIntegral (length scoreString) * 36)

handleInput :: Event -> GameState -> GameState
handleInput (EventMotion (x, _)) game = movePaddle game x
handleInput _ game = game

moveBall :: Float -> GameState -> GameState
moveBall time game = set ball (set ballPosition (x', y') (game ^. ball)) game
  where
    (px, py) = game ^. ball  ^. ballPosition 
    (vx, vy) = game ^. ball  ^. ballVelocity
    x' = px + vx * time
    y' = py + vy * time

brickBounce :: GameState -> IO GameState
brickBounce game = 
  if (brickCollision game HorizontalCollision || brickCollision game VerticalCollision)
    then
      do
        let gameState = game { _ball = set ballVelocity (vx', vy') (game ^. ball), _score = (name, scr), _bricks = removeCollidedBrick (game ^. bricks)}
        putMVar (game ^. collisionSoundChannel) True
        return $ complementBricks gameState
    else
      return $ game
  where
    (oldName, oldScore) = game ^. score
    (vx, vy) = game ^. ball  ^. ballVelocity
    (vx', vy') =  if (brickCollision game HorizontalCollision)
                    then 
                      (-vx, vy)     
                    else
                      if (brickCollision game VerticalCollision) 
                        then
                          (vx, -vy)
                      else
                        (vx, vy)

    (name, scr) = if brickCollision game HorizontalCollision || brickCollision game VerticalCollision
      then
        (oldName, show ((read oldScore ::Int) + 100))
      else
        (oldName, oldScore)
  
    removeCollidedBrick :: [Brick] -> [Brick]
    removeCollidedBrick [] = []
    removeCollidedBrick (x:xs) 
        | (game ^. ball  ^. ballPosition) `isIntersection` (x ^. brickPosition) = xs
        | otherwise  = x : removeCollidedBrick xs 
        
complementBricks :: GameState -> GameState
complementBricks game = if lastRow game == brickRows - 1
                          then 
                            game
                          else
                            complementBricks . addFirstRow . moveRowsDown $ game
  where 
    moveRowsDown :: GameState -> GameState
    moveRowsDown game = 
      game { _bricks = (map (\brick -> brick { _brickPosition = (fst $ brick ^. brickPosition, brickPositionY (brick ^. row  + 1))
                                             , _picture      = translate (fst $ brick ^. brickPosition) (brickPositionY (brick ^. row  + 1)) $ color (brick ^. brickColor) $ rectangleSolid brickWidth brickHeight
                                             , _row          = (brick ^. row) + 1
                                             }) (game ^. bricks))}

    addFirstRow :: GameState -> GameState
    addFirstRow game = 
      game { _bricks = (game ^. bricks) ++ [ Brick { _brickPosition = (brickPositionX j, brickPositionY 0)
                                                 , _picture      = translate (brickPositionX j) (brickPositionY 0) $ color (getColor (game ^. nextRowColor)) $ rectangleSolid brickWidth brickHeight
                                                 , _row          = 0
                                                 , _brickColor    = getColor $ game ^. nextRowColor
                                                 } | j <- [0..brickCols-1]] 
          , _nextRowColor = ((game ^. nextRowColor) - 1) `mod` 5}

    lastRow :: GameState -> Int
    lastRow game = maximum $ map (\brick -> brick ^. row) (game ^. bricks)

paddleBounce :: GameState -> GameState
paddleBounce game = set ball (set ballVelocity (vx', vy') (game ^. ball)) game
  where
    angle = ((-180) * (1 - alpha)) + (180 * alpha)
    alpha = ((fst (game ^. ball  ^. ballPosition)) - (fst (game ^. paddle ^. paddlePosition))) / paddleWidth
    
    (vx, vy) = game ^. ball  ^. ballVelocity
    (vx', vy') = if (game ^. ball ^. ballPosition) `isPaddleCollision` (game ^. paddle ^. paddlePosition) 
      then
        (sin(alpha) * ballFullVelocity , cos(alpha) * ballFullVelocity)
      else
        (vx, vy)

wallBounce :: GameState -> GameState
wallBounce game = set ball (set ballVelocity (vx', vy') (game ^. ball)) game
  where
    (vx, vy) = game ^. ball  ^. ballVelocity
    vx' = if wallCollision game LeftCollision  || wallCollision game RightCollision
      then
        -vx
      else
        vx
    vy' = if wallCollision game TopCollision
      then
        -vy
      else
        vy

checkEndGame :: IO GameState -> IO GameState  
checkEndGame gameIO = do
  game <- gameIO
  if wallCollision game BottomCollision
    then
      do
        return $ set currentView GameOver game
  else
    return game

movePaddle :: GameState -> Float -> GameState
movePaddle game mousePositionX = 
  game { _paddle = (game ^. paddle) { _paddlePosition =
    if checkLeftWindowConstraint game mousePositionX && checkRightWindowConstraint game mousePositionX
      then (mousePositionX, y)
      else 
        if not(checkLeftWindowConstraint game mousePositionX)
          then 
            (-fromIntegral screenWidth / 2 + halfPaddleWidth, y)
          else 
            ( fromIntegral screenWidth / 2 - halfPaddleWidth, y)
        , _paddleVelocityX = mousePositionX - fst (game ^. paddle ^. paddlePosition)
  }}
  where
    y = snd $ game ^. paddle ^. paddlePosition
    
    checkLeftWindowConstraint :: GameState -> Float -> Bool
    checkLeftWindowConstraint game mousePositionX = mousePositionX - halfPaddleWidth >= -fromIntegral screenWidth / 2

    checkRightWindowConstraint :: GameState -> Float -> Bool
    checkRightWindowConstraint game mousePositionX = mousePositionX + halfPaddleWidth <= fromIntegral screenWidth / 2

getColor :: Int -> Color
getColor row
  | (row `mod` 5) == 0 = red 
  | (row `mod` 5) == 1 = green  
  | (row `mod` 5) == 2 = blue  
  | (row `mod` 5) == 3 = yellow  
  | (row `mod` 5) == 4 = white    