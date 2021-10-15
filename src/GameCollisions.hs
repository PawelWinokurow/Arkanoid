{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module GameCollisions (module GameCollisions) where
import Settings
import Geometry
import DataObjects
import Control.Lens
import Control.Concurrent

data CollisionDirection = LeftCollision | TopCollision | RightCollision | BottomCollision | VerticalCollision | HorizontalCollision

wallCollision :: GameState -> CollisionDirection -> Bool
wallCollision game direction = case direction of LeftCollision   -> isLeftCollision   (game ^. ball ^. ballPosition)
                                                 TopCollision    -> isTopCollision    (game ^. ball ^. ballPosition)
                                                 RightCollision  -> isRightCollision  (game ^. ball ^. ballPosition)
                                                 BottomCollision -> isBottomCollision (game ^. ball ^. ballPosition)
  where 
    (x, y) = game ^. ball  ^. ballPosition

brickCollision :: GameState -> CollisionDirection -> Bool 
brickCollision game direction =     
  case direction of HorizontalCollision -> checkHorizontalCollision (game ^. bricks) (game ^. ball ^. ballPosition)
                    VerticalCollision   -> checkVerticalCollision   (game ^. bricks) (game ^. ball ^. ballPosition)
  where 
    checkHorizontalCollision :: [Brick] -> Position -> Bool
    checkHorizontalCollision [] _ = False
    checkHorizontalCollision (x:xs) ballPosition = 
      if ballPosition `isBallBrickHorizontalCollision` (x ^. brickPosition) 
        then
          True
        else
          checkHorizontalCollision xs ballPosition 
    
    checkVerticalCollision :: [Brick] -> Position -> Bool
    checkVerticalCollision [] _ = False
    checkVerticalCollision (x:xs) ballPosition = 
      if ballPosition `isBallBrickVerticalCollision` (x ^. brickPosition) 
        then
          True
        else
          checkVerticalCollision xs ballPosition

class CollisionCheck a where
  isBallBrickVerticalCollision   :: a -> a -> Bool
  isBallBrickHorizontalCollision :: a -> a -> Bool
  isPaddleCollision             :: a -> a -> Bool
  isLeftCollision               :: a -> Bool
  isTopCollision                :: a -> Bool
  isRightCollision              :: a -> Bool
  isBottomCollision             :: a -> Bool

instance CollisionCheck Position where
  isLeftCollision   ballPosition = (fst ballPosition) - ballRadius <= -fromIntegral screenWidth  / 2 
  isTopCollision    ballPosition = (snd ballPosition) + ballRadius >=  fromIntegral screenHeight / 2
  isRightCollision  ballPosition = (fst ballPosition) + ballRadius >=  fromIntegral screenWidth  / 2 
  isBottomCollision ballPosition = (snd ballPosition) - ballRadius <= -fromIntegral screenHeight / 2 

  isBallBrickVerticalCollision ballPosition brickPosition  =
    ballPosition `isIntersection` brickPosition
    && ((ballPosition `distY` brickPosition) / (ballPosition `distX` brickPosition)) >= 0.5

  isBallBrickHorizontalCollision ballPosition brickPosition  = 
    ballPosition `isIntersection` brickPosition
    && ((ballPosition `distY` brickPosition) / (ballPosition `distX` brickPosition)) < 0.5

  isPaddleCollision ballPosition paddlePosition =
    ballPositionY - ballRadius <= paddlePositionY + paddleHeight / 2 
    && ballPositionX >= paddlePositionX - halfPaddleWidth
    && ballPositionX <= paddlePositionX + halfPaddleWidth
      where 
        ballPositionX = fst ballPosition
        ballPositionY = snd ballPosition
        paddlePositionX = fst paddlePosition
        paddlePositionY = snd paddlePosition

class Intersection a where
  isIntersection   :: a -> a -> Bool

instance Intersection Position where
  isIntersection ballPosition brickPosition =
    ballPosition `distX` brickPosition < ballRadius + (brickWidth / 2)  && ballPosition `distY` brickPosition < ballRadius + (brickWidth / 2)