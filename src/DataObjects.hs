{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module DataObjects (module DataObjects) where

import Geometry
import Textures
import Settings
import Graphics.Gloss
import Control.Lens
import Control.Concurrent

data View = MainMenu | GameInProgress | GameExit | GameOver | HighScores
  deriving Eq

data Brick = Brick
  { _brickPosition :: Position
  , _picture       :: Picture
  , _row           :: Int
  , _brickColor    :: Color
  }
makeLenses ''Brick

data BallState = Ball 
  { _ballPosition    :: Position
  , _ballVelocity    :: Position   
  }
makeLenses ''BallState

data PaddleState = Paddle 
  { _paddlePosition  :: Position       
  , _paddleVelocityX :: Float          
  }
makeLenses ''PaddleState

data HighScores = Scores
  { _save   :: Bool
  , _scores :: [(String, String)]
  }
makeLenses ''HighScores

data ButtonsHover = Buttons
  { _isNewGameButtonHovered    :: Bool
  , _isHighScoreButtonHovered  :: Bool
  , _isReplayButtonHovered     :: Bool
  , _isSaveButtonHovered       :: Bool
  , _isExitGameButtonHovered   :: Bool
  , _isBackButtonHovered       :: Bool
  , _isMainMenuButtonHovered   :: Bool
  , _isFirstUpButtonHovered    :: Bool
  , _isFirstDownButtonHovered  :: Bool
  , _isSecondUpButtonHovered   :: Bool
  , _isSecondDownButtonHovered :: Bool
  , _isThirdUpButtonHovered    :: Bool
  , _isThirdDownButtonHovered  :: Bool
  }
makeLenses ''ButtonsHover

data GameState = Game
  { _ball                     :: BallState       
  , _paddle                   :: PaddleState
  , _score                    :: (String, String)       
  , _textures                 :: Textures
  , _bricks                   :: [Brick]
  , _currentView              :: View
  , _exit                     :: Bool
  , _buttonsHover             :: ButtonsHover
  , _highScores               :: HighScores
  , _collisionSoundChannel    :: MVar Bool
  , _nextRowColor             :: Int
  }
makeLenses ''GameState

class CursorInsideButton a where
  insideButton :: a -> a -> Bool

instance CursorInsideButton Position where
  insideButton mousePosition buttonPosition =
    fst mousePosition >= fst buttonPosition + (-buttonWidth) / 2 
    && fst mousePosition <= fst buttonPosition + buttonWidth / 2    
    && snd mousePosition >= snd buttonPosition + (- buttonHeight) / 2    
    && snd mousePosition <= snd buttonPosition + buttonHeight / 2 

class CursorInsideArrowButton a where
  insideArrowButton :: a -> a -> Bool

instance CursorInsideArrowButton Position where
  insideArrowButton mousePosition buttonPosition =
    fst mousePosition >= fst buttonPosition + (-arrowButtonWidth) / 2 
    && fst mousePosition <= fst buttonPosition + arrowButtonWidth / 2    
    && snd mousePosition >= snd buttonPosition + (- arrowButtonHeight) / 2    
    && snd mousePosition <= snd buttonPosition + arrowButtonHeight / 2 