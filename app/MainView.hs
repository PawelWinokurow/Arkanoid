{-# LANGUAGE TemplateHaskell #-}

module MainView (module MainView) where

import GameView(initGameState)
import Textures
import DataObjects
import Settings
import Geometry
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Control.Lens

update :: Float -> GameState -> GameState
update time game = game 

render :: GameState  -> Picture
render game 
  | game ^. buttonsHover ^. isNewGameButtonHovered   = pictures $ [ game ^. textures ^. backgroundMainTex , translate 0 70 $ game ^. textures ^. newGameButtonOnHoverTex , game ^. textures ^. highScoreButtonTex , translate 0 (-70) $ game ^. textures ^. exitGameButtonTex  ]
  | game ^. buttonsHover ^. isHighScoreButtonHovered = pictures $ [ game ^. textures ^. backgroundMainTex , translate 0 70 $ game ^. textures ^. newGameButtonTex , game ^. textures ^. highScoreButtonOnHoverTex , translate 0 (-70) $ game ^. textures ^. exitGameButtonTex  ]
  | game ^. buttonsHover ^. isExitGameButtonHovered  = pictures $ [ game ^. textures ^. backgroundMainTex , translate 0 70 $ game ^. textures ^. newGameButtonTex , game ^. textures ^. highScoreButtonTex , translate 0 (-70) $ game ^. textures ^. exitGameButtonOnHoverTex  ]
  | otherwise                                        = pictures $ [ game ^. textures ^. backgroundMainTex , translate 0 70 $ game ^. textures ^. newGameButtonTex , game ^. textures ^. highScoreButtonTex , translate 0 (-70) $ game ^. textures ^. exitGameButtonTex  ]

handleInput :: Event -> GameState -> GameState
handleInput (EventMotion (x, y)) game = mouseMotionHandler game (x,y)
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) game = mouseClickHandler game (x,y)
handleInput _ game = game

mouseClickHandler :: GameState -> Position -> GameState
mouseClickHandler game mousePosition 
  | game ^. buttonsHover ^. isNewGameButtonHovered   = startGame game
  | game ^. buttonsHover ^. isHighScoreButtonHovered = set currentView HighScores game
  | game ^. buttonsHover ^. isExitGameButtonHovered  = set exit True game
  | otherwise                                        = set currentView MainMenu game 
          
startGame :: GameState -> GameState
startGame game = GameView.initGameState game

mouseMotionHandler :: GameState -> Position -> GameState
mouseMotionHandler game mousePosition = game {_buttonsHover = (_buttonsHover game)  { _isNewGameButtonHovered   = mousePosition `insideButton` (0, 70)  
                                                                                    , _isHighScoreButtonHovered = mousePosition `insideButton` (0, 0)   
                                                                                    , _isExitGameButtonHovered  = mousePosition `insideButton` (0,(-70))  
                                                                                    } }