{-# LANGUAGE TemplateHaskell #-}

module Main where

import Textures
import Settings
import Geometry
import DataObjects
import qualified GameView
import qualified GameOverView
import qualified MainView
import qualified HighScoreView
import Sound
import Store
import Control.Concurrent
import Sound.ProteaAudio
import System.Exit
import Data.Char 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Control.Lens

window :: Display
window = InWindow "Arkanoid" (screenWidth, screenHeight) (screenOffset, screenOffset)

main :: IO ()
main = do
  textures      <- loadTextures
  highScores    <- loadHighScores
  collisionSoundChannel    <- newEmptyMVar
  result <- initAudio 64 44100 1024
  brickCollision    <- sampleFromFile "brickCollision.wav" 1.0
  sequence_ [forkIO $ playSound brickCollision collisionSoundChannel i | i <- [0..numberSoundThreads]]
  let gameState = initGameState textures highScores collisionSoundChannel
  playIO window white fps gameState render handleInput update
 
initGameState :: Textures -> [(String, String)] -> MVar Bool -> GameState
initGameState textures highScores collisionSoundChannel = Game { _textures     = textures
                                                      , _currentView  = MainMenu 
                                                      , _buttonsHover = Buttons { _isNewGameButtonHovered    = False
                                                                                , _isHighScoreButtonHovered  = False
                                                                                , _isReplayButtonHovered     = False
                                                                                , _isSaveButtonHovered       = False
                                                                                , _isExitGameButtonHovered   = False
                                                                                , _isBackButtonHovered       = False
                                                                                , _isMainMenuButtonHovered   = False
                                                                                , _isFirstUpButtonHovered    = False
                                                                                , _isFirstDownButtonHovered  = False
                                                                                , _isSecondUpButtonHovered   = False
                                                                                , _isSecondDownButtonHovered = False
                                                                                , _isThirdUpButtonHovered    = False
                                                                                , _isThirdDownButtonHovered  = False
                                                                                }
                                                      , _exit                     = False
                                                      , _collisionSoundChannel    = collisionSoundChannel
                                                      , _highScores               = Scores { _save = False, _scores = highScores }
                                                      }
        
update :: Float -> GameState -> IO GameState
update time game
  | game ^. currentView  == MainMenu =  if game ^. exit           
                                          then           
                                            do  
                                              exitWith ExitSuccess
                                              return $ MainView.update time game                                                                                                             
                                          else
                                            if game ^. highScores ^. save
                                              then
                                                do 
                                                  forkIO $ saveHighScore $ game ^. highScores  ^.  scores
                                                  return $ MainView.update time game {_highScores = (_highScores game) {_save = False}}                                                                                                            
                                              else
                                                return $ MainView.update time game   

  | game ^. currentView  == GameInProgress  = GameView.update time game
  | game ^. currentView  == GameOver        = return $ GameOverView.update time game
  | game ^. currentView  == HighScores      = return $ HighScoreView.update time game
  
render :: GameState  -> IO Picture
render game 
  | game ^. currentView  == MainMenu        = return $ MainView.render game                                                                                                                  
  | game ^. currentView  == GameInProgress  = return $ GameView.render game
  | game ^. currentView  == GameOver        = return $ GameOverView.render game
  | game ^. currentView  == HighScores      = return $ HighScoreView.render game

handleInput :: Event -> GameState -> IO GameState
handleInput event game
  | game ^. currentView  == MainMenu       = return $ MainView.handleInput event game
  | game ^. currentView  == GameInProgress = return $ GameView.handleInput event game
  | game ^. currentView  == GameOver       = return $ GameOverView.handleInput event game
  | game ^. currentView  == HighScores     = return $ HighScoreView.handleInput event game