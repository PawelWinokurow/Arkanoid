{-# LANGUAGE TemplateHaskell #-}

module HighScoreView(module HighScoreView) where

import Textures
import DataObjects
import Settings
import Geometry
import System.IO  
import System.Exit
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Control.Lens

update :: Float -> GameState -> GameState
update time game = game 

render :: GameState  -> Picture
render game
  | game ^. buttonsHover ^. isBackButtonHovered = pictures $ [ game ^. textures ^. backgroundHighScoreTex , translate 0 (-180) $ game ^. textures ^. backButtonOnHoverTex ] ++ (showHighScores game)
  | otherwise                                   = pictures $ [ game ^. textures ^. backgroundHighScoreTex , translate 0 (-180) $ game ^. textures ^. backButtonTex ] ++ (showHighScores game)    

showHighScores :: GameState -> [Picture]
showHighScores game = [translate 0 (100 + (-50) * fromIntegral row) $ (showHighScoreLine row) | row <- [0..(length $ game ^. highScores ^. scores) - 1]]
  where
    showHighScoreLine :: Int -> Picture
    showHighScoreLine row = pictures $  [translate (-180) 0 (convertStringToPicture $ fst $ (game ^. highScores ^. scores)!!row), translate 110 0 (convertStringToPicture $ snd $ (game ^. highScores ^. scores)!!row)]

    convertStringToPicture :: String -> Picture
    convertStringToPicture text = pictures $ [ translate (fromIntegral (col * 38)) 0 (snd $ (filter (\symbol -> (fst symbol) == text!!col)  (game ^. textures ^. symbolsTex))!!0) | col <- [0..length text - 1]]

handleInput :: Event -> GameState -> GameState
handleInput (EventMotion (x, y)) game = mouseMotionHandler game (x, y)
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) game = mouseClickHandler game (x, y)
handleInput _ game = game

mouseClickHandler :: GameState -> Position -> GameState
mouseClickHandler game mousePosition 
  | game ^. buttonsHover ^. isBackButtonHovered = set currentView MainMenu game
  | otherwise                                   = set currentView HighScores game

mouseMotionHandler :: GameState -> Position -> GameState
mouseMotionHandler game mousePosition = game { _buttonsHover = (_buttonsHover game)  { _isBackButtonHovered = mousePosition `insideButton` (0,(-180)) } }